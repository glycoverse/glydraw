# Save fixed-size glycan cartoon image to local device.

Save fixed-size glycan cartoon image to local device.

## Usage

``` r
save_cartoon(cartoon, filename, path, dpi = 300)
```

## Arguments

- cartoon:

  ggplot2 object

- filename:

  file name of glycan cartoon

- path:

  save path

- dpi:

  dots per inch, default = 300

## Value

NULL, this function is for image saving.

## Examples

``` r
cartoon <- draw_cartoon("Gal(b1-3)GalNAc(a1-")
save_cartoon(cartoon, "p1.png", tempdir(), dpi = 300)
```
