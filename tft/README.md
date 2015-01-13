## Question 1

The problem as stated cannot be solved without making significant assumtions.

- "each wallet can have only 1 record within any hour":
Does this mean within each hour of the clock or within an hour of each
other?

- How do you define a "duplicate": the same wallet_id, or also the same
  balance?

Either way the answer is likely related to the SQL problem of "selecting
the Nth item when applying a GROUP BY clause (where N>1)", but using a
DELETE statement. There are various solutions involving sub-selects,
procedures or temporary-tables but probably the simplest is described in
the following StackOverflow comment:

http://stackoverflow.com/a/5016434/393583

## Question 2

See "tt-rss-reader.py"

## Question 3
    
See "operator-list.html"

## Question 4
    
In MySQL:

    SELECT u.id, u.name, u.registered_on
    FROM users AS u
    LEFT JOIN transaction AS t
    ON u.id = t.user_id
    WHERE u.registered_on >= DATE_SUB(NOW(), INTERVAL 7 DAY)
    AND t.id IS NULL
    GROUP BY u.id;

Just be aware that NOW() returns a timestamp in the current timezone.

