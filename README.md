# Juliano

Fortran program that simulates the Maden Julian Oscillation (MJO). It is based
on the exercises from the book "Modern Fortran" by Milan Curcic. The book
focuses on simulating the behavior of a tsunami using Fortran. Since the model
is based on the wave equations, I thought of adapting it to simulate the
behavior of the MJO using a very nice blog post from
[climate.gov](https://www.climate.gov/news-features/blogs/enso/what-mjo-and-why-do-we-care)
as a reference.

The code is divided into several branches just as a way to document the
progress through each chapter of the book. In that sense, it has more of an
educational purpose than a real-world application.

## Part 1

This part includes the implementation of the wave function in 1 dimension
(longitude). Since the MJO has a positive and negative phase, I simulated two
waves, one with negative value and 90° out of phase.

Some details about the simulation:

- Cyclic boundaries by simulating all longitudes (1° resolution)
- A total of 45 days of simulation, which is one full MJO cycle
- Simulation at hourly scales
