Icons
=====

## Image Sources (2019-02-16)

wget https://github.com/encharm/Font-Awesome-SVG-PNG/raw/master/black/svg/feed.svg
wget https://github.com/encharm/Font-Awesome-SVG-PNG/raw/master/black/svg/github.svg
wget https://github.com/encharm/Font-Awesome-SVG-PNG/raw/master/black/svg/linkedin.svg
wget https://github.com/encharm/Font-Awesome-SVG-PNG/raw/master/black/svg/twitter.svg


## Image Sources (2009-02-20)

https://fontawesome.com/icons/rss?style=solid
https://fontawesome.com/icons/moon?style=solid
https://fontawesome.com/icons/github?style=brands
https://fontawesome.com/icons/twitter?style=brands
https://fontawesome.com/icons/linkedin-in?style=brands


## Trim SVG

- Open image with Inkscape.
- Select the logo in the image and select *File* > *Export PNG Image*.
- Click *Page* and note down *Height* as `H`.
- Click *Selection* and note down *Width* and Height* as `w` and `h`.
- Click *Selection* and note down *x0* and *y1* as `x` and `y`. The `y`
  values increase as we move from bottom of the image to top of the
  image.
- Compute the `viewBox` of SVG as follows: (x, H - y, w, h).
- Edit the SVG image source code to update its `width`, `height`, and
  `viewBox` attributes as per the values computed in the previous step.


## Simplify SVG
