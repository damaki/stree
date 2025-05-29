import os
from pathlib import Path
import shutil

_this_file_dir = Path(os.path.realpath(os.path.dirname(__file__)))

def pytest_addoption(parser):
    parser.addoption(
        "--tests-dir",
        default=str(_this_file_dir / "tests"),
        help="The directory containing the test case subdirectories",
    )
    parser.addoption(
        "--build-dir",
        default=str(_this_file_dir / "build"),
        help="The directory for storing intermediate build/proof artifacts during testing",
    )
    parser.addoption(
        "--support-dir",
        default=str(_this_file_dir / "support"),
        help="The directory containing support files for the tests",
    )
    parser.addoption(
        "--gnatprove-jobs",
        default=1,
        type=int,
        help="Set -j flag passed to GNATprove"
    )
    parser.addoption(
        "--no-clean",
        default=False,
        action="store_true",
        help="Don't delete any existing test case build directories"
    )

def get_tests_dir(request) -> Path:
    return Path(request.config.getoption("--build-dir"))

def get_build_dir(request) -> Path:
    return Path(request.config.getoption("--build-dir"))

def get_support_dir(request) -> Path:
    return Path(request.config.getoption("--support-dir"))

def get_gnatprove_jobs(request) -> int:
    return request.config.getoption("--gnatprove-jobs")

def get_no_clean(request) -> bool:
    return request.config.getoption("--no-clean")