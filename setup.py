import io
import sys
import glob

from setuptools import find_packages, setup

with io.open('calysto_scheme/scheme.py', encoding="utf-8") as fid:
    for line in fid:
        if line.startswith('__version__'):
            __version__ = line.strip().split()[-1][1:-1]
            break

with open('README.md') as f:
    readme = f.read()

setup(name='calysto_scheme',
      version=__version__,
      description='A Scheme kernel for Jupyter that can use Python libraries',
      long_description=readme,
      author='Douglas Blank',
      author_email='doug.blank@gmail.com',
      url="https://github.com/Calysto/calysto_scheme",
      install_requires=["metakernel"],
      packages=find_packages(include=["calysto_scheme", "calysto_scheme.*"]),
      package_data={'calysto_scheme': ["images/*.png", "modules/*.ss"]},
      platforms=["Any"],
      data_files=[
          ('share/jupyter/kernels/calysto_scheme',
           ['calysto_scheme/kernel.json'] + glob.glob('calysto_scheme/images/*.png')
          )
      ],
      classifiers = [
          'Framework :: IPython',
          'License :: OSI Approved :: BSD License',
          'Programming Language :: Python :: 3',
          'Programming Language :: Python :: 2',
          'Programming Language :: Scheme',
          'Topic :: System :: Shells',
      ]
)
