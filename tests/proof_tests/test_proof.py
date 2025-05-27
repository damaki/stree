import conftest
import os
import pytest
import shutil
import subprocess
from pathlib import Path
from string import Template
from typing import Mapping

_this_file_dir = Path(os.path.realpath(os.path.dirname(__file__)))
_tests_dir = _this_file_dir / "tests"


def create_file_from_template(
    template_filename: Path, dst_filename: Path, mapping: Mapping[str, object]
):
    """Copy a template file and fill in the template with the provided mapping."""
    with open(template_filename, "r") as template_file:
        src_data = Template(template_file.read())

    dst_data = src_data.substitute(mapping)
    with open(dst_filename, "w") as dst_file:
        dst_file.write(dst_data)


class TestCase:
    __test__ = False

    def __init__(
        self,
        request: pytest.FixtureRequest,
        test_case_dir: Path,
        build_dir: Path,
    ):
        self._request = request
        self._test_case_dir = test_case_dir
        self._build_dir = build_dir
        self._support_dir = conftest.get_support_dir(request)

    def setup(self):
        """Setup a test case in the build directory.

        This sets up an Alire manifest and GPR file for the test case in the
        build directory.
        """

        # Remove any previous build artifacts
        if self._build_dir.exists():
            shutil.rmtree(self._build_dir)

        # Create the build directory
        self._build_dir.mkdir(exist_ok=False, parents=True)

        # Copy the Alire manifest
        create_file_from_template(
            template_filename=self._support_dir / "alire.toml.tmpl",
            dst_filename=self._build_dir / "alire.toml",
            mapping={
                "stree_dir": str(
                    _this_file_dir.parent.parent.relative_to(
                        self._build_dir, walk_up=True
                    )
                ).replace("\\", "/")
            },
        )

        # Copy GPR file
        create_file_from_template(
            template_filename=self._support_dir / "proof_test.gpr.tmpl",
            dst_filename=self._build_dir / "proof_test.gpr",
            mapping={"src_dir": self._test_case_dir},
        )

    def build(self):
        """Execute "alr build" on a test case.

        This checks for any build errors in the test case.

        Returns 0 on success or any other value on error.
        """
        cp = subprocess.run(
            args=["alr", "build", "--validation"],
            cwd=self._build_dir,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )

        with open(self._build_dir / "build.stdout", "wb") as f:
            f.write(cp.stdout)
        with open(self._build_dir / "build.stderr", "wb") as f:
            f.write(cp.stderr)

        return cp.returncode

    def prove(self):
        """Run GNATprove on a test case."""

        args = [
            "alr",
            "exec",
            "--",
            "gnatprove",
            "-P",
            "proof_test.gpr",
            "--level=2",
            "--no-subprojects",
            "--report=all",
            "--warnings=error",
        ]

        j = conftest.get_gnatprove_jobs(self._request)
        if j != 1:
            args.append(f"-j{j}")

        cp = subprocess.run(
            args=args,
            cwd=self._build_dir,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )

        with open(self._build_dir / "prove.stdout", "wb") as f:
            f.write(cp.stdout)
        with open(self._build_dir / "prove.stderr", "wb") as f:
            f.write(cp.stderr)

        if conftest.get_baseline(self._request):
            with open(self._test_case_dir / "prove.expected.stderr", "wb") as f:
                f.write(cp.stderr)
        else:
            with open(self._test_case_dir / "prove.expected.stderr", "rb") as f:
                assert cp.stderr.decode() == f.read().decode()

        return cp.returncode

@pytest.mark.parametrize(
    "test_dir", [subdir for subdir in _tests_dir.iterdir()], ids=lambda dir: dir.name
)
def test_proof(request: pytest.FixtureRequest, test_dir: Path):
    """Parameterised test to build and prove each test case directory"""

    tc_build_dir = Path(conftest.get_build_dir(request)) / test_dir.name

    tc = TestCase(
        request=request,
        test_case_dir=test_dir,
        build_dir=tc_build_dir,
    )

    tc.setup()

    res = tc.build()
    assert res == 0

    res = tc.prove()
    assert res == 0
