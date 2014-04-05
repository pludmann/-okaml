let rec ratio_order (a1,b2,d1,c1,l1) (a2,b2,d2,c2,l2) =
(float_of_int(l1)/.float_of_int(c1))>(float_of_int(l2)/.float_of_int(c2));;

rStreets = Sort.list ratio_order streets;;


let rec cost_order (a1,b2,d1,c1,l1) (a2,b2,d2,c2,l2) =
(c1)<=(c2);;

cStreets = Sort.list cost_order streets;;

