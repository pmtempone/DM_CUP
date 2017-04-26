-- valores distintos

SELECT  distinct day,pid,"adFlag",availability,"competitorPrice",click,price,manufacturer,"group","content",unit,"pharmForm","genericProduct","salesIndex",category,"campaignIndex",rrp
into public.train_distinct
FROM public.train_v1
order by 1,2;


-- variables lag

SELECT day, pid,lag("competitorPrice") OVER w as lag_competiroprice,
  lag(price) OVER w as lag_price
 into lag_files
FROM   train_v1
WINDOW w AS (PARTITION BY pid
             ORDER BY train_v1
             ROWS BETWEEN 1 PRECEDING AND current row);



#variables avg

SELECT  distinct day,pid,"competitorPrice",price
into public.train_distinct
FROM public.train_v1
order by 1,2;

select day,pid, avg("competitorPrice") avg_comp_price,avg(price) avg_price
from train_distinct
group by day, pid;


select day,pid, avg("competitorPrice") avg_comp_price,avg(price) avg_price
into lkp_avg_price
from train_v1
group by day, pid;

select a.*, avg_comp_price,avg_price
into train_v2
from train_v1 a left join lkp_avg_price b on (a.day=b.day and a.pid=b.pid);

#variables 

SELECT day, pid,lag(avg_comp_price) OVER w as lag_avg_comp_price,
  lag(avg_price) OVER w as lag_avg_price
 into lag_avg
FROM   lkp_avg_price
WINDOW w AS (PARTITION BY pid
             ORDER BY lkp_avg_price
             ROWS BETWEEN 1 PRECEDING AND current row);

---train v3

select a.*, lag_avg_comp_price,lag_avg_price
into train_v3
from train_v2 a left join lag_avg b on (a.day=b.day and a.pid=b.pid);
