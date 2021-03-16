## Create Table
create table danger (
	dno number(25),
	age number(10), 
	sex varchar2(10), 
	d_date varchar2(20), 
	m_item varchar2(100), 
	item varchar2(100), 
	cause varchar2(100), 
	d_sym varchar2(100), 
	loc varchar2(100), 
	body varchar2(100) 
	
);


## Create Danger_age View
create view danger_age as

	select cause as "원인",
			m_item as "품목",
			d_sym as "증상",
			body as "부위",
			loc as "장소",
			count(case when age < 10 then 1 end) "0-9세",
			count(case when age between 10 and 19 then 1 end) "10-19세",
			count(case when age between 20 and 29 then 1 end) "20-29세",
			count(case when age between 30 and 39 then 1 end) "30-39세",
			count(case when age between 40 and 49 then 1 end) "40-49세",
			count(case when age between 50 and 59 then 1 end) "50-59세",
			count(case when age between 60 and 69 then 1 end) "60-69세",
			count(case when age between 70 and 79 then 1 end) "70-79세",
			count(case when age >=80 THEN 1 end) "80세 이상"
		from DANGER
		GROUP BY cause, m_item, d_sym, body, loc;


## Unpivot (Column to Data)
create view danger_age_up as
   
	select * 
		from danger_age
		unpivot 
		(cnt for 연령대 in 
			("0-9세", "10-19세", "20-29세", "30-39세", "40-49세", "50-59세", "60-69세", "70-79세", "80세 이상")
		);


## Create Rank View
create view rank as
  
	select *
		from	
			(select 연령대,
					품목,
					sum(cnt)||'건' as "해당품목건수",
					dense_rank () over (partition by 연령대 order by sum(cnt) desc) 순위
					from danger_age_up
					group by 연령대, 품목)
		where 순위=1;


## Create Reason & Part View
create view 원인 as
     
	select 품목, 연령대, listagg(원인||'('||건수||'건'||')', ', ') within group (order by 원인순위 asc) 상위원인
		from	
			(select 품목,
					원인,
					연령대,
					건수,
					dense_rank () over (partition by 품목, 연령대 order by 건수 desc) 원인순위
					from
						(select 품목, 원인, 연령대, sum(cnt) as "건수"
							from danger_age_up
							group by 원인, 품목,연령대)
					)
		where 원인순위<=3 and 건수 !=0
		group by 품목, 연령대;
  
  
create view 부위 as   
 
	select 품목, 연령대, listagg(부위||'('||건수||'건'||')', ', ') within group (order by 부위순위 asc) 상위부위
		from	
			(select 품목,
					부위,
					연령대,
					건수,
					dense_rank () over (partition by 품목, 연령대 order by 건수 desc) 부위순위
					from	
						(select 품목, 부위, 연령대, sum(cnt) as "건수"
							from danger_age_up
							group by 부위, 품목,연령대)
					)
		where 부위순위<=3 and 건수 !=0 and 부위 is not null and 부위 !='해당없음'
		group by 품목, 연령대;
  
      
## Join
select r.연령대, r.품목, c.상위원인, b.상위부위, r.해당품목건수, (select sum(cnt)||'건' as "전체 건수" from danger_age_up where 연령대=r.연령대) as "전체건수"
	from rank r, 원인 c, 부위 b
	where r.연령대=c.연령대 and r.품목=c.품목 and r.연령대=b.연령대 and r.품목=b.품목;	
	