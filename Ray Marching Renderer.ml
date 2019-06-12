(*----------------------------------------------------------*)
(*--                      CONSTANTES                      --*)
(*----------------------------------------------------------*)

let window_title = "Ray Marching Renderer";;
let img_width = 500;;
let img_height = 400;;

(*----------------------------------------------------------*)
(*--                       IMPORTS                        --*)
(*----------------------------------------------------------*)

#load "graphics.cma";;
module G = Graphics;;

(*----------------------------------------------------------*)
(*--                        TYPES                         --*)
(*----------------------------------------------------------*)

type pos = float * float * float;;
(* un objet est un estimateur de distance *)
type obj = pos -> float;;
type light = ();;

type world = {
	objects: obj array;
	lights: light array;
};;

type camera = {
	position: pos;
	orientation: float*float;
	lookat: pos;
	screen_x: float;
	screen_y: float;
	screen_dist: float;
};;

type ray_marching_result = {
	steps: int;
	pos: pos;
};;

(*----------------------------------------------------------*)
(*--                         UTILS                        --*)
(*----------------------------------------------------------*)

let pi = 3.1415926535897932;;

let (%.) (a:float) (b:float) =
	let rem = mod_float a b in
	if rem >= 0. then
		rem
	else
		b +. rem
;;

let distance (x,y,z) =
	sqrt ( x**2. +. y**2. +. z**2. )
;;

let normalize (x, y, z:pos) =
	let norm = distance (x, y, z) in
	x /. norm, y /. norm, z /. norm
;;

let dot_product (x_, y_, z_:pos) (x, y, z:pos) =
	x_ *. x +. y_ *. y +. z_ *. z
;;

let cross_product (x_, y_, z_:pos) (x, y, z) =
	y_ *. z -. z_ *. y, z_ *. x -. x_ *. z_, x_ *. y -. y_ *. x
;;

(* u, v, w coordonnées du vecteur servant d'axe *)
let rotation (u, v, w:pos) angle (x, y, z:pos) =
	let q, q_i, q_j, q_k = cos (angle /. 2.), 

;;

let m4_transform () =
	let m = Array.make_matrix 4 4 0. in
	m.(3).(3) <- 1.;
	m
;;

let m4_set_column m (x, y, z:pos) i =
	m.(i).(0) <- x;
	m.(i).(1) <- y;
	m.(i).(2) <- z;
;;

let m4_v3_prod m (x, y, z:pos) :pos =
	let v = [| x; y; z; 1. |]
	and v_ = [| 0.; 0.; 0.; 0. |] in
	for i = 0 to 3 do
		for j = 0 to 3 do
			v_.(i) <- v_.(i) +. m.(i).(j) *. v.(j)
		done
	done;
	v_.(0), v_.(1), v_.(2)
;;

(*----------------------------------------------------------*)
(*--                        OBJETS                        --*)
(*----------------------------------------------------------*)

let object_sphere r (x_, y_, z_:pos) (x, y, z:pos)  =
	distance (x -. x_, y -. y_, z -. z_) -. r
;;

let object_half_space (x, y, z:pos) =
	x > 0.
;;

let object_plane (a, b, c, d) (x, y, z:pos) = 
	abs_float (dot_product (a, b, c) (x, y, z) +. d) /. distance (a, b, c)
;;

let object_line (x, y, z:pos) =
	distance (0., y, z) -. 0.1
;;

let object_cuboid (size_x, size_y, size_z) (x_, y_, z_:pos) (x, y, z:pos) = 
	distance (
		max 0. (abs_float (x -. x_) /. size_x *. 2. -. 1.),
		max 0. (abs_float (y -. y_) /. size_y *. 2. -. 1.),
		max 0. (abs_float (z -. z_) /. size_z *. 2. -. 1.)
	)
;;

let cmp_x (x, y, z:pos) = x;;
let cmp_y (x, y, z:pos) = y;;
let cmp_z (x, y, z:pos) = z;;

let ( +! ) (x, y, z:pos) (x_, y_, z_:pos) :pos =
	x +. x_, y +. y_, z +. z_
;;

let ( -! ) (x, y, z:pos) (x_, y_, z_:pos) :pos =
	x -. x_, y -. y_, z -. z_
;;

let ( *! ) f (x_, y_, z_:pos) :pos =
	f *. x_, f *. y_, f *. z_
;;

let ( /! ) f (x_, y_, z_:pos) =
	f /. x_, f /. y_, f /. z_
;;

let object_rec_tetrahedron (x, y, z:pos) =
	let scale = 5.
	and iterations = 10
	and a1 = 1., 1., 1.
	and a2 = -1., -1., 1.
	and a3 = 1., -1., -1.
	and a4 = -1., 1., -1. in
	let rec aux (x, y, z:pos) = function
		0 	-> x, y, z
	|	n	->	let d, c = min
					(distance ((x, y, z) -! a1), a1) 
					(	min
							(distance ((x, y, z) -! a2), a2)
							(	min
									(distance ((x, y, z) -! a3), a3)
									(distance ((x, y, z) -! a4), a4)
							)
					) in
				aux ( scale *! (x, y, z) -! ((scale -. 1.) *! c)) (n - 1)
	in distance (aux (x, y, z) iterations) *. scale ** -. float_of_int iterations
;;

(*----------------------------------------------------------*)
(*--                    TRANSFORMATIONS                   --*)
(*----------------------------------------------------------*)

let union dist_a dist_b =
	min dist_a dist_b
;;

let intersection dist_a dist_b =
	max dist_a dist_b
;;

let difference dist_a dist_b =
	max dist_a (-. dist_b)
;;

let transform_fold_space fold_index (x, y, z:pos) =
	let half = fold_index /. 2. in
	mod_float x fold_index -. half, mod_float y fold_index -. half, mod_float z fold_index -. half
;;
let transform_fold_xy fold_index (x, y, z:pos) =
	x %. fold_index, y %. fold_index, z
;;

(*----------------------------------------------------------*)
(*--                        RENDU                         --*)
(*----------------------------------------------------------*)

let distance_estimator world pos =
	Array.fold_left (function min_dist -> function obj ->
			min min_dist (obj pos)
		) infinity world.objects
;;

let min_hit_distance = 0.001;;
let ray_marching world (ray_x, ray_y, ray_z) start_pos =
	let rec aux (x, y, z:pos) = function
		255 	-> {steps = 255; pos = x, y, z}
	|	n		-> let dist = distance_estimator world (x, y, z) in
					if dist < min_hit_distance then
						{steps = n; pos = x, y, z}
					else begin
						let new_x = x +. dist *. ray_x
						and new_y = y +. dist *. ray_y
						and new_z = z +. dist *. ray_z in
						aux (new_x, new_y, new_z) (n+1)
					end
	in aux start_pos 0
;;

let renderer world camera img_width img_height =
	let img = Array.make_matrix img_height img_width G.black in
	G.set_color G.blue;
	G.moveto 0 0;
	G.lineto img_width 0;
	G.set_color G.red;
	for i = 0 to img_width - 1 do
		for j = 0 to img_height - 1 do
		(*
			(* un ray est un vecteur unitaire donnant la direction du rayon *)
			let theta, phi = camera.orientation in
			let u = camera.screen_dist
			and v = ( float_of_int i /. float_of_int img_width -. 0.5 ) *. camera.screen_x
			and w = ( float_of_int j /. float_of_int img_height -. 0.5 ) *. camera.screen_y in
			let vector_x = u *. cos theta -. v *. sin theta *. cos phi +. w *. sin theta *. sin phi
			and vector_y = u *. sin theta +. v *. cos theta *. cos phi -. w *. cos theta *. sin phi
			and vector_z = v *. sin theta +. w *. cos theta in
			
			let norm = distance (vector_x, vector_y, vector_z) in
			let ray = vector_x /. norm, vector_y /. norm, vector_z /. norm in
		*)
			let xaxis = normalize camera.lookat in
			let yaxis = normalize (cross_product (0., 0., 1.) xaxis) in
			let zaxis = cross_product xaxis yaxis in
			let view_transform = m4_transform () in
			m4_set_column view_transform xaxis 0;
			m4_set_column view_transform yaxis 1;
			m4_set_column view_transform zaxis 2;
			let u = camera.screen_dist
			and v = ( float_of_int i /. float_of_int img_width -. 0.5 ) *. camera.screen_x
			and w = ( float_of_int j /. float_of_int img_height -. 0.5 ) *. camera.screen_y in
			let ray = m4_v3_prod view_transform (u, v, w) in
			(* distance estimator *)
			let result = ray_marching world ray camera.position in
			let steps = if result.steps > 255 then 0 else 255 - result.steps in
			img.(img_height - 1 - j).(i) <- G.rgb steps steps steps;
		done;
		G.plot i 0;
	done;
	G.draw_image (G.make_image img) 0 0
;;

let ( >> ) f g x = f (g x);;

(*----------------------------------------------------------*)
(*--                       FENETRE                        --*)
(*----------------------------------------------------------*)

let window_configuration () =
	string_of_int img_width ^ "x" ^
	string_of_int img_height ^ "+100+100"
;;

let my_world = {
	objects = [| function pos -> difference (object_cuboid (2., 2., 2.) (0., 0., 0.) pos) (object_sphere 1.3 (0., 0., 0.) pos) |];
	lights = [||]
};;
	
(* let my_world = {
	objects = [| (* object_rec_tetrahedron *) object_cuboid (1., 3., 0.725) (0.5, 3., -0.125)
					 (* object_line *) >> transform_fold_xy 6. |];
	lights = [||]
};; *)
let debug_world = {
	objects = [| object_line |];
	lights = [||]
};;
let my_camera = {
	position = -5.5, 2., 0.;
	orientation = 0., 0.;
	lookat = 1., 0.5, 0.;
	screen_x = 0.5;
	screen_y = 0.4;
	screen_dist = 0.5;
};;

G.open_graph (window_configuration ());;

renderer my_world my_camera img_width img_height;;
