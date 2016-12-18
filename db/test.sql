SELECT attribute.attr_name,attr_values.attr_value 
	from item_attrs, attr_values, attribute 
	WHERE item_attrs.item_id=1 
	  AND item_attrs.attr_value_id=attr_values.id 
	  AND attr_values.attr_id=attribute.id;
