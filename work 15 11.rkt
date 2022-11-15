(require minikanren)
(define (appendo l s out)
  (conde
    [(== l '()) (== s out)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res))]))
(define (caro p
a)
(fresh (d)
(== (cons a
d) p)))
(define (cdro p d)
(fresh (a)
(== (cons a d) p)))
(define (membero x l)
(conde
((caro l x))
((fresh (d)
(cdro l d)
(membero x
d)))))
(define (nullo x)
(== '() x))
(define succeed (== #f #f))
(define (init_sched s) ; - расписание группы на неделю
  (fresh (a1 a2 a3 a4 a5 a6 a7 a8 a9 b1 b2 b3 b4 b5 b6 b7 b8 b9 c1 c2 c3 c4 c5 c6) (== s `((,a1,a2,a3,a4)(,a5,a6,a7,a8)(,b1,b2,b3,b4)(,b5,b6,b7,b8)(,c1,c2,c3,c4)))))

(define (init_manysched a1 a2 a3 a4 a5 a6 a7 a8)
  (fresh (b1 b2 b3 b4 b5 b6 b7 b8) (init_sched b1) (init_sched b2) (init_sched b3) (init_sched b4) (init_sched b5) (init_sched b6) (init_sched b7) (init_sched b8) (== a1 b1) (== a2 b2) (== a3 b3) (== a4 b4) (== a5 b5) (== a6 b6) (== a7 b7) (== a8 b8)))

;как выглядят основные объекты: schedallgroup
;teachersched
;schedclass
;можно как написать, беру каку. то функцию на самой схеме, одним из конъюнктов которой будет сама проверка на возможность вставить нужную пару в нужное место
;(define (insertfri1 subjteacher teachersched subj schedonegroup classes schedclass)

(define (ins1 subj schedgroup teachersched schedclass) ;в функцию передается расписание уже на определенный день, после чего на первую пару вставляется предмет
  (conde
   [(fresh (a1 b1 c1)
     (caro schedgroup a1) (== a1 subj)
     (caro teachersched b1) (== b1 subj)
     (caro schedclass c1) (== c1 subj)
           )]
   ))

(define (ins2 subj schedgroup teachersched schedclass) 
  (conde
   [(fresh (a1 b1 c1 a2 b2 c2)
     (cdro schedgroup a1) (caro a1 a2) (== a2 subj)
     (cdro teachersched b1) (caro b1 b2) (== b2 subj)
     (cdro schedclass c1) (caro c1 c2) (== c2 subj)
           )]
   ))
(define (ins3 subj schedgroup teachersched schedclass) 
  (conde
   [(fresh (a1 b1 c1 a2 b2 c2 a3 b3 c3)
     (cdro schedgroup a1) (cdro a1 a2) (caro a2 a3) (== a3 subj)
     (cdro teachersched b1) (cdro b1 b2) (caro b2 b3) (== b3 subj)
     (cdro schedclass c1) (cdro c1 c2) (caro c2 c3) (== c3 subj)
           )]
   ))
(define (ins4 subj schedgroup teachersched schedclass) 
  (conde
   [(fresh (a1 b1 c1 a2 b2 c2 a3 b3 c3 a4 b4 c4)
 (cdro schedgroup a1) (cdro a1 a2) (cdro a2 a3) (caro a3 a4) (== a4 subj)
     (cdro teachersched b1) (cdro b1 b2) (cdro b2 b3) (caro b3 b4) (== b4 subj)
     (cdro schedclass c1) (cdro c1 c2) (cdro c2 c3) (caro c3 c4) (== c4 subj)
           )]
   ))


(define (insmonday subj schedgroup teachersched schedclass) ; пытается вставить предмет в понедельник в одну из пар
 (fresh (a1 a2 b1 b2 c1 c2)
    (caro schedgroup a1)
    (caro teachersched b1) 
    (caro schedclass c1)
    (conde
   [(ins1 subj a1 b1 c1)]       
   [(ins2 subj a1 b1 c1)]  
   [(ins3 subj a1 b1 c1)] 
   [(ins4 subj a1 b1 c1)]
   )))

(define (instuesday subj schedgroup teachersched schedclass)
  (fresh (a1 a2 b1 b2 c1 c2)
    (cdro schedgroup a1) (caro a1 a2) ;возвращает вторник в расписании группы
    (cdro teachersched b1) (caro b1 b2)
    (cdro schedclass c1) (caro c1 c2)
  (conde
   [(ins1 subj a2 b2 c2)]
   [(ins2 subj a2 b2 c2)]
   [(ins3 subj a2 b2 c2)]   
   [(ins4 subj a2 b2 c2)])))

(define (inswednesday subj schedgroup teachersched schedclass)
  (fresh (a1 a2 b1 b2 c1 c2 a3 b3 c3)
    (cdro schedgroup a1) (cdro a1 a2) (caro a2 a3) 
    (cdro teachersched b1) (cdro b1 b2) (caro b2 b3)
    (cdro schedclass c1) (cdro c1 c2) (caro c2 c3)
  (conde
   [(ins1 subj a3 b3 c3)]
   [(ins2 subj a3 b3 c3)]
   [(ins3 subj a3 b3 c3)]   
   [(ins4 subj a3 b3 c3)])))

(define (insthursday subj schedgroup teachersched schedclass)
  (fresh (a1 a2 b1 b2 c1 c2 a3 b3 c3 a4 b4 c4)
    (cdro schedgroup a1) (cdro a1 a2) (cdro a2 a3) (caro a3 a4) 
    (cdro teachersched b1) (cdro b1 b2) (cdro b2 b3) (caro b3 b4)
    (cdro schedclass c1) (cdro c1 c2) (cdro c2 c3) (caro c3 c4)
  (conde
   [(ins1 subj a4 b4 c4)]
   [(ins2 subj a4 b4 c4)]
   [(ins3 subj a4 b4 c4)]   
   [(ins4 subj a4 b4 c4)])))

(define (insfriday subj schedgroup teachersched schedclass)
  (fresh (a1 a2 b1 b2 c1 c2 a3 b3 c3 a4 b4 c4 a5 b5 c5)
    (cdro schedgroup a1) (cdro a1 a2) (cdro a2 a3) (cdro a3 a4) (caro a4 a5)
    (cdro teachersched b1) (cdro b1 b2) (cdro b2 b3) (cdro b3 b4) (caro b4 b5)
    (cdro schedclass c1) (cdro c1 c2) (cdro c2 c3) (cdro c3 c4) (caro c4 c5)
  (conde
   [(ins1 subj a5 b5 c5)]
   [(ins2 subj a5 b5 c5)]
   [(ins3 subj a5 b5 c5)]   
   [(ins4 subj a5 b5 c5)])))

(define (insall subj schedgroup teachersched schedclass)
  (conde
   [(insmonday subj schedgroup teachersched schedclass)]
   [(instuesday subj schedgroup teachersched schedclass)]
   [(inswednesday subj schedgroup teachersched schedclass)]
   [(insthursday subj schedgroup teachersched schedclass)]
   [(insfriday subj schedgroup teachersched schedclass)]
   ))

;(define (insall2 subj schedgroup teachersched schedclass)
 ; (fresh (a1 a2 b1 b2 c1 c2 a3 b3 c3 a4 b4 c4 a5 b5 c5)
  ;  (cdro schedgroup a1) (cdro a1 a2) (cdro a2 a3) (cdro a3 a4) (caro a4 a5)
   ; (cdro teachersched b1) (cdro b1 b2) (cdro b2 b3) (cdro b3 b4) (caro b4 b5)
    ;(cdro schedclass c1) (cdro c1 c2) (cdro c2 c3) (cdro c3 c4) (caro c4 c5)
 ; (conde
   
   
  
;(define (timetableonegroup studyplan schedgroup teachersched schedclass classessubj);здесь происходит пробег по аудиториям
 ; (conde
  ; [(== studyplan '()) succeed]
   ;[(conde
    ; [(fresh(a1 a2 a3 a4 a5) (caro studyplan a1) (caro teachersched a3) (caro classessubj a4) (membero a1 a4) (caro schedclass a5) (insall a1 schedgroup a3 a5))]
     ;[(fresh (b1 b2 b3) (cdro classessubj b1) (cdro schedclass b2) (timetableonegroup studyplan schedgroup teachersched b2 b1))])
      ;   (fresh(a2 a4) (cdro studyplan a2) (cdro teachersched a4) (timetableonegroup a2 schedgroup a4 schedclass classessubj))
    ;]
   ;))

(define (timetableonegroup2 studyplan schedgroup teachersched schedclass classessubj)
  (conde
   [(== studyplan '()) succeed]
   [(fresh (a1 a2 a3 a4 a5 b1 b2) (caro studyplan a1) (caro teachersched a2) (caro classessubj a3) (membero a1 a3) (caro schedclass a5) (insall a1 schedgroup a2 a5)
          (cdro studyplan b1) (cdro teachersched b2) (timetableonegroup2 b1 schedgroup b2 schedclass classessubj))
           ]
   [(fresh (a1 a2 a3 a4 a5 a6) (cdro classessubj a1) (cdro schedclass a2) (timetableonegroup2 studyplan schedgroup teachersched a2 a1))]
   ))

(define (sched studyplanallgroup schedallgroup allteachersched schedclass classessubj) ;составляет расписание на все группы, но только практики
  (conde
   [(== studyplanallgroup '()) succeed]
   [(fresh (a1 a2 a3) (caro studyplanallgroup a1) (caro schedallgroup a2) (caro allteachersched a3) (timetableonegroup2 a1 a2 a3 schedclass classessubj))
    (fresh (a4 a5 a6) (cdro studyplanallgroup a4) (cdro schedallgroup a5) (cdro allteachersched a6) (sched     a4 a5 a6 schedclass classessubj))
           ]
   ))

(define (inslectire1 subj teachersched first second third fourth schedclass)
  (conde
   [(caro teachersched subj)
    (caro first subj)
    (caro second subj)
    (caro third subj)
    (caro fourth subj)]
    (caro schedclass subj)
   ))

(define (inslectire2 subj teachersched first second third fourth schedclass)
(fresh (a1 a2 a3 a4 a5 a6 a7 a8 a9)
  (conde
   [(cdro teachersched a1) (caro a1 subj)
    (cdro first a2) (caro a2 subj)
    (cdro second a3) (caro a3 subj)
    (cdro third a4) (caro a4 subj)
    (cdro fourth a5) (caro a5 subj)
    (cdro schedclass a6) (caro a6 subj)
    ] 
   )))

(define (inslectire3 subj teachersched first second third fourth schedclass)
(fresh (a1 a2 a3 a4 a5 a6 b6 b1 b2 b3 b4 b5)
  (conde
   [(cdro teachersched a1) (cdro a1 b1) (caro b1 subj)
    (cdro first a2) (cdro a2 b2) (caro b2 subj)
    (cdro second a3) (cdro a3 b3)(caro b3 subj)
    (cdro third a4) (cdro a4 b4)(caro b4 subj)
    (cdro fourth a5) (cdro a5 b5)(caro b5 subj)
    (cdro schedclass a6) (cdro a6 b6) (caro b6 subj)
    ] 
   )))

(define (inslectire4 subj teachersched first second third fourth schedclass)
(fresh (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5 a6 b6 c6)
  (conde
   [(cdro teachersched a1) (cdro a1 b1) (cdro b1 c1) (caro c1 subj)
    (cdro first a2) (cdro a2 b2) (cdro b2 c2) (caro c2 subj)
    (cdro second a3) (cdro a3 b3)(cdro b3 c3) (caro c3 subj)
    (cdro third a4) (cdro a4 b4)(cdro b4 c4) (caro c4 subj)
    (cdro fourth a5) (cdro a5 b5)(cdro b5 c5) (caro c5 subj)
    (cdro schedclass a6) (cdro a6 b6) (cdro b6 c6) (caro c6 b6)
    ] 
   )))

(define (inslecmonday subj teachersched first second third fourth schedclass)
  (fresh (a1 a2 a3 a4 a5 a6)
         (caro teachersched a1)
         (caro first a2)
         (caro second a3)
         (caro third a4)
         (caro fourth a5)
         (caro schedclass a6)
         (conde
          [(inslectire1 subj a1 a2 a3 a4 a5 a6)]
          [(inslectire2 subj a1 a2 a3 a4 a5 a6)]
          [(inslectire3 subj a1 a2 a3 a4 a5 a6)]
          [(inslectire4 subj a1 a2 a3 a4 a5 a6)]
          )))

(define (inslectuesday subj teachersched first second third fourth schedclass)
  (fresh (a1 a2 a3 a4 a5 a6 b1 b2 b3 b4 b5 b6)
         (cdro teachersched b1) (caro b1 a1)
         (cdro first b2) (caro b2 a2)
         (cdro second b3) (caro b3 a3)
         (cdro third b4) (caro b4 a4)
         (cdro fourth b5) (caro b5 a5)
         (cdro schedclass b6) (caro b6 a6)
         (conde
          [(inslectire1 subj a1 a2 a3 a4 a5 a6)]
          [(inslectire2 subj a1 a2 a3 a4 a5 a6)]
         ; [(inslectire3 subj a1 a2 a3 a4 a5)]
          ;[(inslectire4 subj a1 a2 a3 a4 a5)]
          )))

(define (inslecall subj teachersched first second third fourth schedclass)
  (conde
   [(inslecmonday subj teachersched first second third fourth schedclass)]
   [(inslectuesday subj teachersched first second third fourth schedclass)]
   ))

(define (lec studyplanlec first second third fourth allteachersched schedclass classessubj) 
  (conde
   [(== studyplanlec '()) succeed]
   [(fresh (a1 a2 a3 a4) (caro studyplanlec a1) (caro allteachersched a2) (caro classessubj a3) (membero a1 a3) (caro schedclass a4) (inslecall a1 a2 first second third fourth a4))
    (fresh(b1 b2 b3) (cdro studyplanlec b1) (cdro allteachersched b2) (lec b1 first second third fourth b2 schedclass classessubj))]
    
    [(fresh (a1 a2 a3) (cdro classessubj a1) (cdro schedclass a2) (lec studyplanlec first second third fourth allteachersched a2 a1))]
   ))


(define (inslecmon1 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 cl1 cl2)
       (caro first a1) (caro a1 a2) (== a2 lecture)
       (caro second b1) (caro b1 b2) (== b2 lecture)
       (caro third c1) (caro c1 c2) (== c2 lecture)
       (caro fourth v1) (caro v1 v2) (== v2 lecture)
       (caro teachersched x1) (caro x1 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
       (caro schedclass z1) (caro z1 z2) (caro z2 z3) (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1) (cdro classes n2) (inslecmon1 lecture teachersched first second third fourth n2 n1))]
    ))

(define (inslecmon2 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 s1 s2 s3 s4 s5 s6 cl1 cl2)
       (caro first a1) (cdro a1 s1) (caro s1 a2) (== a2 lecture)
       (caro second b1) (cdro b1 s2) (caro s2 b2) (== b2 lecture)
       (caro third c1) (cdro c1 s3) (caro s3 c2) (== c2 lecture)
       (caro fourth v1) (cdro v1 s4) (caro s4 v2) (== v2 lecture)
       (caro teachersched x1) (cdro x1 s5) (caro s5 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
       (caro schedclass z1) (caro z1 z2) (cdro z2 s6) (caro s6 z3) (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1)(cdro classes n2) (inslecmon2 lecture teachersched first second third fourth n2 n1))]
    ))
         
(define (inslecmon3 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 s1 s2 s3 s4 s5 s6 d1 d2 d3 d4 d5 d6 cl1 cl2)
       (caro first a1) (cdro a1 s1) (cdro s1 d1) (caro d1 a2) (== a2 lecture)
       (caro second b1) (cdro b1 s2) (cdro s2 d2) (caro d2 b2) (== b2 lecture)
       (caro third c1) (cdro c1 s3) (cdro s3 d3) (caro d3 c2) (== c2 lecture)
       (caro fourth v1) (cdro v1 s4) (cdro s4 d4) (caro d4 v2) (== v2 lecture)
       (caro teachersched x1) (cdro x1 s5) (cdro s5 d5) (caro d5 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
       (caro schedclass z1) (caro z1 z2) (cdro z2 s6) (cdro s6 d6) (caro d6 z3) (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1)(cdro classes n2) (inslecmon3 lecture teachersched first second third fourth n2 n1))]
    ))

(define (inslecmon4 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 s1 s2 s3 s4 s5 s6 d1 d2 d3 d4 d5 d6 f1 f2 f3 f4 f5 f6 cl1 cl2)
       (caro first a1) (cdro a1 s1) (cdro s1 d1) (cdro d1 f1) (caro f1 a2) (== a2 lecture)
       (caro second b1) (cdro b1 s2) (cdro s2 d2) (cdro d2 f2) (caro f2 b2) (== b2 lecture)
       (caro third c1) (cdro c1 s3) (cdro s3 d3) (cdro d3 f3) (caro f3 c2) (== c2 lecture)
       (caro fourth v1) (cdro v1 s4) (cdro s4 d4) (cdro d4 f4) (caro f4 v2) (== v2 lecture)
       (caro teachersched x1) (cdro x1 s5) (cdro s5 d5) (cdro d5 f5) (caro f5 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
       (caro schedclass z1) (caro z1 z2) (cdro z2 s6) (cdro s6 d6) (cdro d6 f6) (caro f6 z3) (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1)(cdro classes n2) (inslecmon4 lecture teachersched first second third fourth n2 n1))]
    ))

(define (inslectue1 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 m1 m2 m3 m4 m5 m6 cl1 cl2)
       (cdro first m1) (caro m1 a1) (caro a1 a2) (== a2 lecture)
       (cdro second m2) (caro m2 b1) (caro b1 b2) (== b2 lecture)
       (cdro third m3) (caro m3 c1) (caro c1 c2) (== c2 lecture)
       (cdro fourth m4) (caro m4 v1)(caro v1 v2) (== v2 lecture)
       (cdro teachersched m5) (caro m5 x1) (caro x1 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
       (caro schedclass z1) (cdro z1 m6) (caro m6 z2) (caro z2 z3) (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1)(cdro classes n2) (inslectue1 lecture teachersched first second third fourth n2 n1))]
    ))

(define (inslectue2 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 m1 m2 m3 m4 m5 m6 l1 l2 l3 l4 l5 l6 cl1 cl2)
       (cdro first m1) (caro m1 a1) (cdro a1 l1) (caro l1 a2) (== a2 lecture)
       (cdro second m2) (caro m2 b1) (cdro b1 l2) (caro l2 b2) (== b2 lecture)
       (cdro third m3) (caro m3 c1) (cdro c1 l3) (caro l3 c2) (== c2 lecture)
       (cdro fourth m4) (caro m4 v1)(cdro v1 l4) (caro l4 v2) (== v2 lecture)
       (cdro teachersched m5) (caro m5 x1) (cdro x1 l5) (caro l5 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
      (caro schedclass z1) (cdro z1 m6) (caro m6 z2) (cdro z2 l6) (caro l6 z3)  (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1)(cdro classes n2) (inslectue2 lecture teachersched first second third fourth n2 n1))]
    ))


  
(define (inslecture2 lecture teachersched first second third fourth classes schedclass)
  (conde
   [(inslecmon1 lecture teachersched first second third fourth classes schedclass)]
   [(inslecmon2 lecture teachersched first second third fourth classes schedclass)]
   [(inslecmon3 lecture teachersched first second third fourth classes schedclass)]
   [(inslecmon4 lecture teachersched first second third fourth classes schedclass)]
   [(inslectue1 lecture teachersched first second third fourth classes schedclass)]
   [(inslectue2 lecture teachersched first second third fourth classes schedclass)]
   ))

(define (inslecture1 lecture subjteacher teachersched first second third fourth classes schedclass)
  (conde
   [(fresh (a1 a2 a3) (caro subjteacher a1) (membero lecture a1) (caro teachersched a2) (inslecture2 lecture a2 first second third fourth classes schedclass))]
   [(fresh (b1 b2 b3) (cdro subjteacher b1) (cdro teachersched b2) (inslecture1 lecture b1 b2 first second third fourth classes schedclass))]
   ))


(define (lecturefor4group lectureplan subjteacher teachersched first second third fourth classes schedclass)
  (conde
   [(== lectureplan '()) succeed]
   [(fresh (a1 a2 a3) (caro lectureplan a1) (inslecture1 a1 subjteacher teachersched first second third fourth classes schedclass))
    (fresh (b1 b2 b3) (cdro lectureplan b1) (lecturefor4group b1 subjteacher teachersched first second third fourth classes schedclass))]
   ))

;(run 5 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched b1 b2 b3 b4 b5 b6 b7 b8)
;                  (lecturefor4group '(matanlec alglec) '((matanlec alglec)) `(,b1) b2 b3 b4 a5 '((matanlec alglec)) `(,a6))
                  
 ;                 (== q `(,a2,a3,a4,a5,a1,a6))))

  
;(time (run 10 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8)
 ;                        
  ;                     (sched '((matan geom alg eng)(matan2 eng2)) `(,a1,a4) `((,a2,a2,a2,a2)(,a2,a2)) `(,a3) '((matan alg geom eng matan2 geom2 alg2 eng2)))
   ;               (== q `(,a1,a4,a2,a3)))))

(time (run 1 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8 b07 b08 b09 b10 teachersched schedclass n1 n2 n3 m1 m2 ) (init_sched b5) (init_sched b6) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched b1 b2 b3 b4 b07 b08 b09 b10); за 662
           (== teachersched `(,a1,a2,a3,a4,a5,a6)) (== schedclass `(,b1,b2,b3,b4))
           ;(caro b07 n1) (caro n1 n2) (== n2 '(chill)) (caro b08 m1) (caro m1 m2) (== m2 '(chill))
           (lecturefor4group '(matanlec1 alglec geomlec inflec difflec matanlec2) '((matanlec1 alglec geomlec inflec difflec matanlec2 proglec)) `(,b5) b07 b08 b09 b10 '((matanlec1 alglec geomlec inflec difflec matanlec2 proglec)) `(,b6))
           (sched '((matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan3 matan4 alg2 geom2 diff2 prog2 eng2)  (matan5 matan6 alg3 geom3 diff3 prog3 eng3) (matan7 matan8 alg4 geom4 diff4 prog4 eng4))
                  `(,b07,b08,b09,b10) `((,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)) `(,b1,b2,b3,b4)
                  '((matan1 matan2 alg1 geom1 diff1 prog1 eng1 matanlecture1 matanlecture2 alglecture geomlecture difflecture inflecture)(matan3 matan4 alg2 geom2 diff2 prog2 eng2)(matan5 matan6 alg3 geom3 diff3 prog3 eng3)(matan7 matan8 alg4 geom4 diff4 prog4 eng4)))
          (== q `(,b07,b08,b09,b10,teachersched,b5,schedclass,b6)))))




;работает за 40 секунд, хотя в работе за 08 11 всего за 15-20! надо что-то исправить. тут из-за разницы в подаче преподов в итоге процесс происходит дольше. Преподов я передаю, по идее намного лучше, программа не тратит время на поиск нужного препода, это хорошо
;хотя плюс небольшой, препод ищется за линейное время, вполне возможно, что изется дольше, из-ха того, что плохо бегаю по группам
;посмотреть с 56 страницы доклад уильяма



