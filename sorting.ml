let rec ratio_order (a1,b2,d1,c1,l1) (a2,b2,d2,c2,l2) =
(l1/.c1)<=(l2/.c2);;

rStreets = Sort.array ratio_order streets;;


let rec cost_order (a1,b2,d1,c1,l1) (a2,b2,d2,c2,l2) =
(c1)<=(c2);;

cStreets = Sort.array cost_order streets;;

