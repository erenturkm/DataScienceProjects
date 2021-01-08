create table if not exists testtable (
	Measured Date not null,
	strName character varying(60) not null,
	intAge integer not null,
	intIncome integer not null,
	floatHeight float not null,
	intWeight integer not null 
)