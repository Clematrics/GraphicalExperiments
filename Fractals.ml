(*  commentaire  *)
#load "graphics.cma";;
module G = Graphics;;

let init_window () =
	G.open_graph "800x800+100-100";
;;

type point = int * int;;

let moveto (a:point) = let x,y = a in G.moveto x y;;
let lineto (a:point) = let x,y = a in G.lineto x y;;

let rec dragon (a:point) (b:point) = function
	0 -> begin moveto a; lineto b; end
|	n -> begin
			let ax, ay = a
			and bx, by = b in
			let diffx, diffy = bx - ax, by - ay in
			let transfo = (ax + bx - diffy) / 2, (ay + by + diffx) / 2 in
			dragon a transfo (n-1);
			dragon b transfo (n-1);
		end
;;

let rec flocon (a:point) (b:point) = function
	0 -> begin moveto a; lineto b; end
|	n -> begin
			let ax, ay = a
			and bx, by = b in
			let diffx, diffy = (bx - ax) /2, (by - ay) / 2 in
			let p1 = (ax + ax + bx) / 3, (ay + ay + by) / 3
			and p2 = (ax + bx - diffy) / 2, (ay + by + diffx) / 2
			and p3 = (ax + bx + bx) / 3, (ay + by + by) /3 in
			flocon a p1 (n-1);
			flocon p1 p2 (n-1);
			flocon p2 p3 (n-1);
			flocon p3 b (n-1);
		end
;;

module C = Complex;;

let event_fractal (a:C.t) (b:C.t) =
	let status = G.wait_next_event [G.Button_down; G.Key_pressed] in
	if status.keypressed then
		false
	else if status.button then begin
		print_float (a.re +. (b.re -. a.re) *. (float_of_int status.mouse_x) /. 800.);
		print_newline ();
		print_float (a.im +. (b.im -. a.im) *. (float_of_int status.mouse_y) /. 800.);
		print_newline ();
		true
	end
	else
		true
;;

let fractal func (a:C.t) (b:C.t) max_iter =
	for x=0 to 800 do
		for y=0 to 800 do
			let c:C.t = {re = a.re +. (b.re -. a.re) *. (float_of_int x) /. 800. ; im = a.im +. (b.im -. a.im) *. (float_of_int y) /. 800. } in
			let i = func c max_iter in
			let prop = 255 - i * 255 / max_iter in
			let color = G.rgb prop prop prop in
			G.set_color color;
			G.plot x y;
		done
	done;
	while event_fractal a b do ()
	done
;;

let iter_mandelbrot c max_iter =
	let rec aux z = function
		iter when iter = max_iter -> max_iter
	|	iter ->
			if C.norm z > 10. then
				iter
			else
				aux (C.add (C.mul z z) c) (iter + 1)
	in aux C.zero 0
;;

let mandelbrot = fractal iter_mandelbrot;;

let iter_star c max_iter =
	let rec aux z = function
		iter when iter = max_iter -> max_iter
	|	iter ->
			if C.norm z > 1. then
				iter
			else
				aux (C.add (C.mul (C.exp z) z) c) (iter + 1)
	in aux C.zero 0
;;

let star = fractal iter_star;;

init_window ();;
dragon (300,300) (600,600) 13;;
flocon (0,0) (800,0) 11;;
mandelbrot {re = -1.5; im = -1.} {re = 0.5; im = 1.} 255;;
mandelbrot {re = 0.23; im = -0.61} {re = 0.36; im = -0.48} 255;;
mandelbrot {re = 0.2526; im = -0.5893} {re = 0.2394; im = -0.5761} 255;;
mandelbrot {re = 0.244548; im = -0.576245} {re = 0.24402; im = -0.576745} 255;;
mandelbrot {re = 0.2737125; im = -0.486125} {re = 0.2759875; im = -0.483325} 255;;
mandelbrot {re = 0.27466515625; im = -0.4852045} {re = 0.27492678125; im = -0.4848755} 255;;
mandelbrot {re = 0.27371534375; im = -0.485852} {re = 0.27392578125; im = -0.4855685} 255;;

star {re = -1.5; im = -1.} {re = 0.5; im = 1.} 255;;
star {re = -20.; im = -20.} {re = 20.; im = 20.} 128;;

let is_in_bound i j =
	0 <= i && i < 801 && 0 <= j && j < 801
;;

let pixel2point (a:C.t) (b:C.t) x y :C.t =
	{re = a.re +. (b.re -. a.re) *. (float_of_int x) /. 800. ; im = a.im +. (b.im -. a.im) *. (float_of_int y) /. 800. }
;;

let point2pixel (a:C.t) (b:C.t) (c:C.t) =
	let i = int_of_float (800. *. (c.re -. a.re) /. (b.re -. a.re))
	and j = int_of_float (800. *. (c.im -. a.im) /. (b.im -. a.im))
	in i,j
;;

let inside_cardioids (d:C.t) =
	let q = (d.re -. 0.25) ** 2. +. d.im *. d.im in
	q *. (q +. d.re -. 0.25) < 0.25 *. d.im *. d.im && (d.re +. 1.) ** 2. +. d.im *. d.im < 0.0625 
;;

let rec inject_path arr = function
	[] -> ()
|	(x,y)::t ->
		if is_in_bound x y then
			arr.(x).(y) <- arr.(x).(y) + 1;
		inject_path arr t
;;

#load "Float.cma";;

let mega_log n =
	let f = float_of_int n in
	log (f ** 10.)
;;

let buddhabrot (a:C.t) (b:C.t) max_iter =
	let arr = Array.make_matrix 801 801 0 in
	G.set_color (G.rgb 255 0 0);
	for x = 0 to 800 do
		G.plot x 0;
		for y = 0 to 800 do
			let d:C.t = pixel2point a b x y in
			let rec iter path z = function
				n when n >= max_iter ->
					if C.norm z > 2. then
						true, path
					else
						false, []
			|	n ->
					let c = (C.add (C.mul z z) d) in
					let i,j = point2pixel a b c in
					iter ((i,j)::path) c (n + 1)
			in 
			if not (inside_cardioids d) then
				let res, path = iter [] C.zero 0 in
				if res then 
					inject_path arr path
		done
	done;
	let maxi = ref 0 in
	for x = 0 to 800 do
		for y = 0 to 800 do
			if !maxi < arr.(x).(y) then
				maxi := arr.(x).(y)
		done
	done;
	print_int !maxi;
	print_newline ();
	for x = 0 to 800 do
		for y = 0 to 800 do
			let prop = 255 * arr.(x).(y) / 2 in
			let p = if prop > 255 then 255 else prop in
			let color = G.rgb p p p in
			(*let prop = if arr.(x).(y) > 0 then 255 else 0 in
			let color = G.rgb prop prop prop in*)
			G.set_color color;
			G.plot x y;
		done
	done;(*
	let freq = Array.make 801 0 in
	G.set_color (G.rgb 255 0 0);
	for x = 0 to 800 do
		for y = 0 to 800 do
			let i = 800 * arr.(x).(y) / !maxi in
			freq.(i) <- freq.(i) + 1
		done
	done;
	G.moveto 0 0;
	for x = 0 to 800 do
		G.lineto x freq.(x)
	done*)
	(*while event_fractal a b do ()
	done*)
;;

buddhabrot {re = -1.5; im = -1.} {re = 0.5; im = 1.} 50;;
