README
================
@laneharrison
May 26, 2016

service-imgd-climate is ...

``` r
data <- read.csv('data/data.csv')

key <- data[c(0, 1), ]

data <- data[-c(1, 2), ]
```

Dividing by Majors
==================

| Majors | minors | graduate students | non-majors |
|--------|--------|-------------------|------------|
| 67     | 13     | 11                | 53         |

Dividing by Demographics
========================

Majors, Minors, Graduate Students (n = 91) Non-Majors (n = 53)

<table style="width:25%;">
<colgroup>
<col width="2%" />
<col width="2%" />
<col width="2%" />
<col width="2%" />
<col width="2%" />
<col width="2%" />
<col width="2%" />
<col width="2%" />
<col width="2%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">LGBTQ</th>
<th align="left">Male</th>
<th align="left">Only White/Caucasian</th>
<th align="left">Only Black/African American</th>
<th align="left">Hispanic</th>
<th align="left">Only American Indian/Alaskan Native</th>
<th align="left">Only Other Race</th>
<th align="left">More than One Race</th>
<th align="left">First Generation College Students</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">24.44</td>
<td align="left">75.82</td>
<td align="left">72.53</td>
<td align="left">2.2</td>
<td align="left">5.49</td>
<td align="left">0.00</td>
<td align="left">16.48</td>
<td align="left">0.00</td>
<td align="left">0.00</td>
</tr>
<tr class="even">
<td align="left">7.55</td>
<td align="left">58.49</td>
<td align="left">64.15</td>
<td align="left">1.89</td>
<td align="left">11.32</td>
<td align="left">0.00</td>
<td align="left">18.87</td>
<td align="left">0.00</td>
<td align="left">1.89</td>
</tr>
</tbody>
</table>

Comparison Groupings
====================

Privileged Male: White, Straight, Male AND IMGD Major, Minor, or Graduate Student

| Answer | Frequency | Percent | Cumulative Percent |
|--------|-----------|---------|--------------------|
| No     | 45        | 49.45   | 49.45              |
| Yes    | 46        | 50.55   | 100                |
| -      | -         | -       | -                  |
| Total  | 91        | 100     | 100                |
