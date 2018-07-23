(defun get-inchi-and-dG (filename)
  (tofile filename
	  (format t "FrameId	Name	Kegg	InChI	Smiles	Gibbs-0~%")
	  (loop for cpd in (get-class-all-instances '|Compounds|)
		for inchi = (get-slot-value cpd 'inchi)
		for dG = (get-slot-value cpd 'Gibbs-0)
		for kegg = (get-kegg-id cpd)
		for smiles = (get-slot-value cpd 'smiles)
		when (and dG inchi smiles (not (qm9-unfriendly-p cpd)))
		do (format t "~A	~A	~A	~A	~A	~A~%"
			   (get-frame-name cpd)
			   (get-name-string cpd)
			   kegg
			   inchi
			   smiles
			   dG))))

(defun get-kegg-id (cpd)
  (loop for dblink in (get-slot-values cpd 'dblinks)
	for db = (car dblink)
	for kegg = (cadr dblink)
	when (fequal db 'LIGAND-CPD)
	return kegg))

(defun qm9-unfriendly-p (cpd)
  (loop for (atom num) in (get-slot-values cpd 'chemical-formula)
	if (or (and (equal atom 'C)
		    (> num 9))
	       (not (member atom '(C H O N F))))
	return t))

		
(defun unfriendly-rxn-p (rxn)
  (loop for cpd in (substrates-of-reaction rxn)
	for inchi = (get-slot-value cpd 'inchi)
	for dG = (get-slot-value cpd 'Gibbs-0)
	if (or (not dG)
	       (not inchi)
	       (qm9-unfriendly-p cpd))
	return t))

(defun print-friendly-rxns (filename)
  (tofile filename
	  (format t "FrameId	Name	dG	Equation~%")
	  (loop for rxn in (all-rxns)
		for frame-id = (get-frame-name rxn)
		for name = (get-name-string rxn :rxn-eqn-as-name? nil)
		for dG = (get-slot-value rxn 'gibbs-0)
		for eqn = (get-name-string rxn :rxn-eqn-as-name? t :strip-html? t)
		unless (unfriendly-rxn-p rxn)
		do (format t "~A	~A	~A	~A~%"
			frame-id
			name
			dG
			eqn))))
	

(defun qm9-rxn-p (rxn qm9-cpds)
  (let ((substrates (substrates-of-reaction rxn)))
    (= (length substrates)
       (length (intersection substrates
			     qm9-cpds :test #'fequal)))
    ))

(defun print-qm9-rxns (filename qm9-cpds)
  (tofile filename
	  (format t "FrameId	Name	dG	Equation~%")
	  (loop for rxn in (all-rxns)
		for frame-id = (get-frame-name rxn)
		for name = (get-name-string rxn :rxn-eqn-as-name? nil)
		for dG = (get-slot-value rxn 'gibbs-0)
		for eqn = (get-name-string rxn :rxn-eqn-as-name? t :strip-html? t)
		when (qm9-rxn-p rxn qm9-cpds)
		do (format t "~A	~A	~A	~A~%"
			frame-id
			name
			dG
			eqn))))
  
