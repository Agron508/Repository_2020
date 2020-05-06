**Shiny created by [Gina Nichols](https://vanichols.netlify.app/) and Richard Cirone**
<br>
**Concept developed by Richard Cirone and [Gina Nichols](https://vanichols.netlify.app/)**
<br>
Agron508, Department of Agronomy, Iowa State University, 2020

**Description**:
This app shows the downwelling shortwave radiation high above a soybean canopy at a field site near Ellsworth, IA in 2019.  Data is pulled from select, multi - day periods in July, August, and September. Radiation was measured using an [Apogee SS-110 Field Spectroradiometer-340 to 820 nm](https://www.apogeeinstruments.com/ss-110-field-spectroradiometer-340-to-820-nm/).  Spectral resolution is 1 nm; observations were recorded every 30 seconds.  Data was collected by the VanLocke Lab, and is available upon request (currently in Spectodradiometer CyBox folder).

**Data Processing**:
First, the eight files with shortwave data in July - September were read into an R script.  Negative fluxes and fulxes greater than 3 W/(m^2 * nm) were changed to NaN.  Then the mean hourly radiative flux was calculated.  Photon flux (moles of photons per square meter per second) was also calculated.  Then the mean photon flux for each hour of day for each month was calculated and stored as "condensed.csv".  This is the file that the app reads from.

**Visualization**:
The app plots the photon or energy flux for the selected month, with a plot for a two-hour interval starting at 6am.  The flux is also broken up by 10 nm wavebands, each bar showing the sum of its 10 contributing wavelengths.  There are some issues with the data plotted, such as why there is no flux for many wavebands plotted midday in July and to a lesser extent September (likley all negative values or very large values so removed), and also the occurance of large fluxes in the longest waveband in September. The cause of this bad data is unknown.