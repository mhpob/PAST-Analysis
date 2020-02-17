This README was generated on 20191219 by M. O'Brien.

This README is in reference to the minimal data sets needed to recreate the figures and analyses in "Differential migration in Chesapeake Bay striped bass", submitted by D. Secor to PLOS ONE on 20191216.

-------------------
Dataset: secor_tagging_data.csv
-------------------
Author (Name, Institution, Email):
M. O'Brien, UMCES, obrien@umces.edu

Date of dataset preparation:
20191212

Dates of data collection:
20140330 - 20140411, 20141030

Geographic location of data collection:
20140330 - 20140411: Newburg, Maryland, USA
20141030: Point Lookout State Park, Scotland, Maryland, USA

Funding source: Atlantic States Marine Fisheries Commission

Number of variables: 6

Number of cases/rows: 100; 1 header row + 99 observations

Variables:
"transmitter"
	Character. Full VEMCO V16 transmitter code, used as a fish identifier. Two cases (A69-1601-25465a, A69-1601-25465b) contain a trailing letter, as the same transmitter was re-used in two different fish.

"tag.date"
	Date (MM/DD/YYYY). Date of transmitter implantation into fish.

"length.mm"
	Numeric. Total length of the fish at the time of tagging in millimeters.

"weight.kg"
	Numeric. Weight of the fish at the time of tagging in kilograms.

"sex"
	Character. Identified sex of the fish (M = Male, F = Female). Instances where sex was not identified are blank.

"age.yrs"
	Numeric. Age of fish at the time of tagging in years. Ages were determined through laboratory ageing of scales.





-------------------
Dataset: secor_detections.csv
-------------------
Author (Name, Institution, Email):
M. O'Brien, UMCES, obrien@umces.edu

Dates of data collection:
20140401 - 20181231

Geographic location of data collection:
Coastal Atlantic Ocean, USA from Virginia to Massachusetts
Chesapeake Bay and tributaries
Delaware River
Hudson River
Long Island Sound

Funding source: Atlantic States Marine Fisheries Commission

Number of variables: 5

Number of cases/rows: 9961; 1 header row + 9960 observations

Variables:
"transmitter"
	Character. Full VEMCO V16 transmitter code, used as a fish identifier. Two cases (A69-1601-25465a, A69-1601-25465b) contain a trailing letter, as the same transmitter was re-used in two different fish.

"date"
	Date (YYYY-MM-DD). Date of detection within the given array. Only one detection per day is reported for each transmitter-day-location combination.

"lat"
	Numeric. Latitude of the detection.

"long"
	Numeric. Longitude of the detection.

"array"
	Character. Geographic area into which the detection was grouped. One of:
	"Bay Mouth" (Chesapeake Bay mouth)
	"C&D" (Chesapeake and Delaware Canal)
	"Choptank" (River, Chesapeake Bay tributary)
	"DE Coast" (Coastal Delaware, Atlantic Ocean)
	"Delaware" (River)
	"Elizabeth" (River, Chesapeake Bay tributary)
	"Hudson" (River)
	"James" (River, Chesapeake Bay tributary)
	"Long Island" (Coastal Atlantic Ocean or Long Island Sound)
	"Lower MD Bay" (Section of the Maryland portion of the Chesapeake Bay)
	"Lower Potomac" (Section of the Potomac River, Chesapeake Bay tributary)
	"Mass" (Coastal Massachusetts Atlantic Ocean and associated rivers)
	"MD Coast" (Coastal Maryland, Atlantic Ocean)
	"Mid MD Bay" (Section of the Maryland portion of the Chesapeake Bay)
	"Mid Potomac" (Section of the Potomac River, Chesapeake Bay tributary)
	"NA" (Reported capture by fisher, location unavailable)
	"Nanticoke" (River, Chesapeake Bay tributary)
	"New Jersey" (Coastal New Jersey Atlantic Ocean and associated rivers)
	"NYB" (New York Bight, Atlantic Ocean)
	"Patuxent" (River, Chesapeake Bay tributary)
	"Rappahannock" (River, Chesapeake Bay tributary)
	"Upper MD Bay" (Section of the Maryland portion of the Chesapeake Bay)
	"Upper Potomac" (Section of the Potomac River, Chesapeake Bay tributary)
	"VA Coast" (Coastal Virginia, Atlantic Ocean)
	"York" (River, Chesapeake Bay tributary)

