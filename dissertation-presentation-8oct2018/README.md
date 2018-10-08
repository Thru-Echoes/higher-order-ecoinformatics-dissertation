# Image processing overview / example: Scikit-Image

This repo contains several files:


1. Image processing reference and utility functions in the **util** dir
2. Main Jupyter notebook: **image-process-overview.ipynb**
3. Exploration of images / data Jupyter notebook: **image-processing-overview.ipynb**

### Install and Host Juypter Notebooks

0. Download machine learning Jupyter TensorFlow Docker container (*Dev environment*)

```
    $ sudo docker pull dash00/tensorflow-python3-jupyter
```

1. Open Jupyter notebook Docker container 

```
    $ sudo docker run -it -p 8888:8888 dash00/tensorflow-python3-jupyter
```

2. Issue installing packages Python user (*not Docker Python*)

```
    $ pip install <PACKAGE_NAME> --user
```

3. Run Jupyter notebook Docker container with persistent dir 

```
    $ sudo docker run -it -p 8888:8888 -v /$(pwd)/notebooks:/notebooks dash00/tensorflow-python3-jupyter
```

### References

[Segementation example / tutorial from SciKit-image](http://scikit-image.org/docs/dev/auto_examples/xx_applications/plot_coins_segmentation.html#sphx-glr-auto-examples-xx-applications-plot-coins-segmentation-py)

[Measure region example / tutorial from SciKit-image](http://scikit-image.org/docs/dev/auto_examples/segmentation/plot_regionprops.html#sphx-glr-auto-examples-segmentation-plot-regionprops-py)
