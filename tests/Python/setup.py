import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="dspaceswrapper",
    version="0.0.1",
    author="Zhe Wang",
    author_email="zw241@rutgers.edu",
    description="A package for DataSpaces client",
    long_description="Please refer to http://personal.cac.rutgers.edu/TASSL/projects/data/index.html for more details",
    long_description_content_type="text/markdown",
    url="https://github.com/philip-davis/dataspaces",
    packages=setuptools.find_packages(),
    package_data={'dspaceswrapper': ['*.so']},
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
)

