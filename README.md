# CUTE

This is a configurable convolution accelerator consisting of multiple small outer product matrix multiplication units and small SCratchpads. Modify configuration parameters in the `HWParameters.scala` file.

It can be tightly coupled to the CPU, such as to BOOM in chipyard. This project is based on chipyard 1.2: https://github.com/ucb-bar/chipyard and can be referred to https://www.sciencedirect.com/science/article/abs/pii/S1383762123000012

CUTE can also be used only as a convolution accelerator, by modifying the convTOP.scala file to change the interface to what is needed.

