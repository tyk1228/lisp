;radian �� degrees�ɂ���֐�
(defun Radians->Degrees(nbrOfRadians)
  (* 180.0 (/ nbrOfRadians pi))
  )

;���W�擾��̃|�C���g�ʒu�擾�֐�
(defun PointX(point)
  (car point)
  )
(defun PointY(point)
  (cadr point)
  )

;------------------------------------------------------------------------------
;�p�x�w��֐�
(defun fc(setDegrees)
;  (setq x(getint "���������:")) x->setDegrees
  (if (>= setDegrees 338) ; 338<=x<23 x=0
      (progn
	(setq tn 0)
	)

    (if (>= setDegrees 315) ;293<=x<338 x=315
	(progn
	  (setq tn 315)
	  )

      (if (>= setDegrees 248) ;248<=x<293 x=270
	  (progn
	    (setq tn 270)
	    )

	(if (>= setDegrees 203) ; 203<=x<248 x=225
	    (progn
	      (setq tn 225)
	      )

	  (if (>= setDegrees 158) ; 158<=x<203 x=180
	      (progn
		(setq tn 180)
		)

	    (if (>= setDegrees 113) ; 113<=x<158 x=135
		(progn
		  (setq tn 135)
		  )

	      (if (>= setDegrees 68) ; 68<=x<113 x=90
		  (progn
		    (setq tn 90)
		    )

		(if (>= setDegrees 23) ; 23<=x<68 x=45
		    (progn
		      (setq tn 45)
		      )
		  (setq tn 0)
		  )
		)
	      )
	    )
	  )
	)
      )
    )
  )
;return 0,45,90,135,180,225,270,315
;------------------------------------------------------------------------------
  ;pt2�ʒu����

  ;x�����ɋ�������ꍇ
(defun move_x(set_pt2 px1 px2)
  (setq vec (list (- px1 px2) 0.0 0.0))
  (command "MOVE" set_pt2 "" vec "")
  )

  ;y�����ɋ�������ꍇ
(defun move_y(set_pt2 py1 py2)
  (setq vec (list 0.0 (- py1 py2) 0.0))
  (command "MOVE" set_pt2 "" vec "")
)
;----------------------------------------------------------------------------

;���� COMMAND
(defun fc_side(pt1 pt2 px1 px2 py1 py2 set_pt1 set_pt2)

  ;�p�x�ϊ�
  (setq rad(angle pt1 pt2))
  (setq degrees(Radians->Degrees rad))

  ;�p�x����
  (setq rd (fc degrees))

  ;0=x����,180=x����,90=y����,270=y����
  ;45>=y����(x����),135>=x����(y����),225>=y����(x����),315>=x����(y����)
  (cond
   ((= rd 0)
    (move_y set_pt2 py1 py2)

    )
   ((= rd 45)
    (move_x set_pt2 px1 px2)

    )
   ((= rd 90)
    (move_x set_pt2 px1 px2)

    )
   ((= rd 135)
    (move_y set_pt2 py1 py2)

    )
   ((= rd 180)
    (move_y set_pt2 py1 py2)

    )
   ((= rd 225)
    (move_x set_pt2 px1 px2)

    )
   ((= rd 270)
    (move_x set_pt2 px1 px2)

    )
   ((= rd 315)
    (move_y set_pt2 py1 py2)

    )
   )
  (setq ent_pt2(entget set_pt2))
  (setq pt2(cdr (assoc 10 ent_pt2)))
  ;���̉�w���擾
  (setq l1 (getvar "clayer"))
  ;���C����w�ɕύX
  (setvar "clayer" "_CLINE")
  (command "line" pt1 pt2 "")
  (COMMAND "DRAWORDER" set_pt2 "" "_F")
  (COMMAND "DRAWORDER" set_pt1 "" "_F")
  ;��w�����ɖ߂�)
  (setvar "clayer" l1)
  )



;;main----------------------------------------------------------


;cad command
;(defun c:set_pside()
;  (setq sc (getint "set scale:"))
;)
 
(DEFUN c:pside ()
 (setq dcl_id(load_dialog "c:/lisp/pside.dcl"))
  (new_dialog "ps" dcl_id)
;�X�P�[���̎w��
  (progn
    (setq sc 1)
    (set_tile "d_sc" (itoa sc))
    (action_tile "d_sc" "(setq sc(atoi(get_tile\"d_sc\")))")
    )
  
  (setq chkdia(start_dialog))
  (unload_dialog dcl_id)

  (setvar "clayer" "_CYSIDE")
  (setvar "cmdecho" 0)
  (setq s (getstring "CY-TH=CT,CY-CY=CC,CY-NO=CN,TH-TH=TT,TH-NO=TN,NO-NO=NN:"))
(while (= chkdia 1)
  (cond
   ((= s "ct");CY-TH
    (COMMAND "INSERT"
	   "c:/lisp/bal/cylinder_side/maru.dwg"
	   "S"
	   sc
	   (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")
	   ;(SETQ sc (GETVAR "dimscale"))
	   (setq in_point(GETPOINT))
	   pause
	   )
    (setq set_in(entlast))
    (setq px1(PointX in_point))
    (setq py1(PointY in_point))


    (princ set_in)
    (COMMAND "INSERT"
	  "c:/lisp/bal/cylinder_side/sankaku.dwg"
	  "S"
	  sc
	  (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")
	  (setq out_point(GETPOINT))
	  pause
	  )
    (setq set_out(entlast))
    (setq px2(PointX out_point))
    (setq py2(PointY out_point))


    ;�ړ��R�}���h
    (fc_side in_point out_point px1 px2 py1 py2 set_in set_out)

    )

   ((= s "cc");CY-CY=CC
    (COMMAND "INSERT"
	   "c:/lisp/bal/cylinder_side/maru.dwg"
	   "S"
	   sc
	   (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")

	   ;(SETQ sc (GETVAR "dimscale"))
	   (setq in_point(GETPOINT))
	   pause
	   )
    (setq set_in(entlast))
    (setq px1(PointX in_point))
    (setq py1(PointY in_point))


    (princ set_in)
    (COMMAND "INSERT"
	  "c:/lisp/bal/cylinder_side/maru.dwg"
	  "S"
	  sc
	  (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")

	  (setq out_point(GETPOINT))
	  pause
	  )
    (setq set_out(entlast))
    (setq px2(PointX out_point))
    (setq py2(PointY out_point))


    ;�ړ��R�}���h
    (fc_side in_point out_point px1 px2 py1 py2 set_in set_out)

    )
   
   ((= s "cn");CY-NO=CN
    (COMMAND "INSERT"
	   "c:/lisp/bal/cylinder_side/maru.dwg"
	   "S"
	   sc
	   (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")

	   ;(SETQ sc (GETVAR "dimscale"))
	   (setq in_point(GETPOINT))
	   pause
	   )
    (setq set_in(entlast))
    (setq px1(PointX in_point))
    (setq py1(PointY in_point))


    (princ set_in)
    (COMMAND "INSERT"
	  "c:/lisp/bal/cylinder_side/batu.dwg"
	  "S"
	  sc
	  (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")

	  (setq out_point(GETPOINT))
	  pause
	  )
    (setq set_out(entlast))
    (setq px2(PointX out_point))
    (setq py2(PointY out_point))


    ;�ړ��R�}���h
    (fc_side in_point out_point px1 px2 py1 py2 set_in set_out)

    )

   ((= s "tt");TH-TH=TT
    (COMMAND "INSERT"
	   "c:/lisp/bal/cylinder_side/sankaku.dwg"
	   "S"
	   sc
	   (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")

	   ;(SETQ sc (GETVAR "dimscale"))
	   (setq in_point(GETPOINT))
	   pause
	   )
    (setq set_in(entlast))
    (setq px1(PointX in_point))
    (setq py1(PointY in_point))


    (princ set_in)
    (COMMAND "INSERT"
	  "c:/lisp/bal/cylinder_side/sankaku.dwg"
	  "S"
	  sc
	  (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")

	  (setq out_point(GETPOINT))
	  pause
	  )
    (setq set_out(entlast))
    (setq px2(PointX out_point))
    (setq py2(PointY out_point))


    ;�ړ��R�}���h
    (fc_side in_point out_point px1 px2 py1 py2 set_in set_out)

    )

   ((= s "tn");TH-NO=TN
    (COMMAND "INSERT"
	   "c:/lisp/bal/cylinder_side/sankaku.dwg"
	   "S"
	   sc
	   (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")

	   ;(SETQ sc (GETVAR "dimscale"))
	   (setq in_point(GETPOINT))
	   pause
	   )
    (setq set_in(entlast))
    (setq px1(PointX in_point))
    (setq py1(PointY in_point))


    (princ set_in)
    (COMMAND "INSERT"
	  "c:/lisp/bal/cylinder_side/batu.dwg"
	  "S"
	  sc
	  (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")

	  (setq out_point(GETPOINT))
	  pause
	  )
    (setq set_out(entlast))
    (setq px2(PointX out_point))
    (setq py2(PointY out_point))


    ;�ړ��R�}���h
    (fc_side in_point out_point px1 px2 py1 py2 set_in set_out)

    )

   ((= s "nn");NO-NO=NN
    (COMMAND "INSERT"
	   "c:/lisp/bal/cylinder_side/batu.dwg"
	   "S"
	   sc
	   (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")

	   ;(SETQ sc (GETVAR "dimscale"))
	   (setq in_point(GETPOINT))
	   pause
	   )
    (setq set_in(entlast))
    (setq px1(PointX in_point))
    (setq py1(PointY in_point))


    (princ set_in)
    (COMMAND "INSERT"
	  "c:/lisp/bal/cylinder_side/batu.dwg"
	  "S"
	  sc
	  (getvar "dimscale")
	   "A"
	   (getvar "dimaunit")

	  (setq out_point(GETPOINT))
	  pause
	  )
    (setq set_out(entlast))
    (setq px2(PointX out_point))
    (setq py2(PointY out_point))


    ;�ړ��R�}���h
    (fc_side in_point out_point px1 px2 py1 py2 set_in set_out)
    
    )
   )
  )  
);_ end of command

