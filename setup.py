"""setup.py for legacy 'python setup.py install' compatibility.

Delegates compilation to pyoz.build(), then lets setuptools handle installation.
Prefer: pip install .
"""

import os
import zipfile

from setuptools import Distribution, setup
from setuptools.command.build_ext import build_ext


class PyOZBuildExt(build_ext):
    """Custom build_ext that uses pyoz to compile the Zig extension."""

    def run(self):
        try:
            from _pyoz import build as pyoz_build
        except ImportError:
            raise RuntimeError(
                "pyoz is required to build zgram. Install it with: pip install pyoz"
            )

        wheel_path = pyoz_build(True, True)

        with zipfile.ZipFile(wheel_path, "r") as whl:
            os.makedirs(self.build_lib, exist_ok=True)
            for name in whl.namelist():
                if "/" not in name and name.endswith((".so", ".pyd", ".pyi")):
                    target = os.path.join(self.build_lib, name)
                    with whl.open(name) as src, open(target, "wb") as dst:
                        dst.write(src.read())
                    if name.endswith((".so", ".pyd")):
                        os.chmod(target, 0o755)


class BinaryDistribution(Distribution):
    def has_ext_modules(self):
        return True


setup(
    name="zgram",
    version="0.1.0",
    description="Comptime-optimized PEG parser generator.",
    author="Daniele Linguaglossa",
    url="https://github.com/dzonerzy/zgram",
    license="MIT",
    python_requires=">=3.8",
    zip_safe=False,
    cmdclass={"build_ext": PyOZBuildExt},
    distclass=BinaryDistribution,
)
