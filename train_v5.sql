select *, price/(CASE WHEN "competitorPrice" > 0 THEN  "competitorPrice" ELSE price END) up_price,"adFlag"*availability*"genericProduct" as multiple,
"adFlag"*availability as flag_avail,"adFlag"*"genericProduct" as flag_generic,
availability*"genericProduct" as avail_generic
into train_v5
from train_v4;