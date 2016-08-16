select a.user_id, a.platform_action, b.minute_description
from user_platform_action_facts as a, time_dim as b
where a.time_id = b.idselect A.user_id, A.platform_action, A.minute_description, B.sql_date_stamp
from
  (select a.user_id, a.platform_action, a.date_id, b.minute_description
    from user_platform_action_facts as a, time_dim as b
    where a.time_id = b.id) as A
  , date_dim as B
where A.date_id = B.id


