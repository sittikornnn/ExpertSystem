has(dorcus_bucephalus,large_fangs_curved_outside).
has(dorcus_bucephalus,the_front_half_of_the_pliers_is_facing_down_but_the_rounded_end_is_bent_up).
has(dorcus_bucephalus,size_50_to_80mm).
has(dorcus_bucephalus,shadow_wings).

has(alltopus_moellenkanpi,golden_green_skin).
has(alltopus_moellenkanpi,black_spot_color).
has(alltopus_moellenkanpi,the_first_segment_has_many_colored_spots).
has(alltopus_moellenkanpi,size_50_to_70mm).
has(alltopus_moellenkanpi,black_color).

has(hexarthrius_mandibularis,dark_brown).
has(hexarthrius_mandibularis,slender_body).
has(hexarthrius_mandibularis,long-nose_pliers_are_quite_thick_Almost_as_long_as_the_body).
has(hexarthrius_mandibularis,very_small_teeth_arranged_from_middle_to_tip).
has(hexarthrius_mandibularis,size_49_to_108mm).
has(hexarthrius_mandibularis,the_large_tooth_is_in_the_middle).

has(hexarthrius_nigritus,the_teeth_at_the_end_of_the_pliers_are_small_and_tapered).
has(hexarthrius_nigritus,size_43_to_75mm).
has(hexarthrius_nigritus,black_color).

has(phalacrognathus_muelleri,beautiful_color).
has(phalacrognathus_muelleri,rainbow_reflection).
has(phalacrognathus_muelleri,long_pliers_curved_together).
has(phalacrognathus_muelleri,size_37_to_68mm).
has(phalacrognathus_muelleri,the_tip_of_the_horn_is_two-pointed).

has(dorcus_reichei,the_blade_is_less_curved_and_near_the_tip_there_are_many_teeth).
has(dorcus_reichei,size_24_to_45mm).
has(dorcus_reichei,red_black_color).
has(dorcus_reichei,the_tip_of_the_horn_is_two-pointed).
has(dorcus_reichei,smooth_wings).

has(dorcus_tityus,small_teeth_point_forward).
has(dorcus_tityus,size_21_to_67mm).
has(dorcus_tityus,large_curved_fangs).
has(dorcus_tityus,red_black_color).
has(dorcus_tityus,the_large_tooth_is_in_the_middle).
has(dorcus_tityus,smooth_wings).

has(dorcus_curvidens,big_shape_at_the_front).
has(dorcus_curvidens,small_teeth_at_tip).
has(dorcus_curvidens,size_30_to_75mm).
has(dorcus_curvidens,there_are_12_deep_and_horizontal_grooves_on_the_wing).
has(dorcus_curvidens,large_curved_fangs).
has(dorcus_curvidens,the_large_tooth_is_in_the_middle).
has(dorcus_curvidens,black_color).
has(dorcus_curvidens,shadow_wings).

has(hexarthrius_parryi,tip_of_antenna_4_to_5_segments).
has(hexarthrius_parryi,size_43_to_80mm).
has(hexarthrius_parryi,yellow_wings).

has(dorcus_titanus,large_fangs_curved_outward_especially_near_the_tip).
has(dorcus_titanus,the_teeth_are_arranged_like_a_saw).
has(dorcus_titanus,size_40_to_86mm).
has(dorcus_titanus,smooth_wings).
has(dorcus_titanus,large_curved_fangs).

has(dorcus_antaeus,size_40_to_80mm).
has(dorcus_antaeus,shadow_wings).
has(dorcus_antaeus,the_large_tooth_is_in_the_middle).
has(dorcus_antaeus,black_color).
has(dorcus_antaeus,large_curved_fangs).

has(hexarthrius_vitalisi,long-nose_pliers_curve_inward_and_slightly_downward).
has(hexarthrius_vitalisi,tip_of_antenna_5_to_6_segments).
has(hexarthrius_vitalisi,size_45_to_82mm).


start :-
    write('Type a name beetle or attribute of beetle.'),nl,
    read(A),
    process(A).

process(A) :- has(A,B),output(B,A).
process(A) :- has(B,A),output(A,B).

output(X,Y) :- write(X),write(' is attribute of the '),write(Y),write('.'),nl.
