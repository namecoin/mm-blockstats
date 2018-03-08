;;; mm-blockstats.el --- Statistics on NMC merged-mined blocks
;;;
;;; Copyright (C) 2015
;;;
;;; Author: cassini <info2015@cassini.tv>
;;; Version: 0.1.0
;;;
;;; mm-blockstats.el is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the license, or
;;; (at your option) any later version.
;;;
;;; mm-blockstats.el is distributed without any warranty, and without an implied
;;; warranty of merchantability or fitness for a particular purpose. See the
;;; GNU General Public License for more details: http://www.gnu.org/licenses/
;;;
;;; Prerequisites: a fully synchronized Namecoin Core client running on Linux or Mac OS.
;;; The Namecoin Core client needs to be started with the -server parameter for being accessible via namecoin-cli.
;;;
;;; Usage:
;;;   emacs --batch --script mm-blockstats.el  \
;;;     <from-blockheight>  \
;;;     <to-blockheight>    \
;;;     <report-progress>   \
;;;     <verbose>           \
;;;     <namecoin-cli-string>
;;;
;;; If <to-blockheight> is "false" then <from-blockheight> is treated as <last-n-blocks>, see example below.
;;; If <report-progress> is "true" or a number > 0 then a line with the currently analyzed blockheight is being displayed every n blocks
;;;  (default in case of "true": n=200)
;;; If <verbose> is "true" then one line per block is being displayed, containing some basic info about that block.
;;;
;;; <namecoin-cli-string> should contain pathnames and any extra arguments for your namecoin-cli executable.
;;; This is simply "./namecoin-cli" if this software resides in the same directory as the namecoin-cli executable.
;;; It can be much more complex if needed: "/home/namecoiner/namecoin-cli -conf=/sdcard/shared/namecoin.conf -testnet"
;;;
;;; Examples:
;;;
;;;   emacs --batch --script mm-blockstats.el 260001 270000 200 false "./namecoin-cli"
;;; does the standard statistics for block 260001 to 270000. Progress report every 200 blocks. No verbosity.
;;;
;;;   emacs --batch --script mm-blockstats.el 100 false false true "./namecoin-cli"
;;; verbosely shows the data of the last 100 blocks, one line per block.
;;;
;;;
;;; Compatible with Emacs V22.x, V23.x, V24.x
;;; Tested on V22.1 (OSX-built-in; no need to install anything else if run on OSX)
;;;


(progn
  (require 'cl)
  (setq max-mini-window-height 0.90) ; in case someone wants to run this software from within Emacs.


  (defun mm-blockstats-main (arg-from arg-to arg-report-progress arg-verbose-p arg-namecoin-cli-string)

    (let (loc-from
	  loc-to
	  loc-report-progress-p
	  loc-verbose-p

	  loc-getblockcount-resultstring
	  loc-current-blockheight

	  loc-block
	  loc-blockhash
	  loc-blockhash-stripped

	  loc-hexstring
	  loc-hash160
	  loc-coinbaseversion
	  loc-meaningful

	  loc-results-plist
	  loc-results-alist
	  loc-number-of-analyzed-blocks

	  loc-blocksplit-step1
	  loc-blocksplit-step2
	  loc-blocksplit-step3
	  loc-blocksplit-step4
	  loc-txsplit-step0
	  loc-txsplit-step1
	  loc-txsplit-step2
	  loc-txsplit-step3)

      ;; Check if namecoin-cli works, and fetch current blockheight for the option the <arg-from> argument needs to be interpreted as "last n blocks"
      (setq loc-getblockcount-resultstring (shell-command-to-string (concat arg-namecoin-cli-string " getblockcount")))
      (setq loc-current-blockheight (string-to-number loc-getblockcount-resultstring))

      (if (< loc-current-blockheight 1)
	  (progn (princ (format "An error occurred while testing the namecoin client. The test command was: %s" (concat arg-namecoin-cli-string " getblockcount")))
		 (terpri)
		 (princ (format "Result: %s" loc-getblockcount-resultstring))
		 (terpri)
		 (princ "Please check your namecoin.conf file AND your namecoin-cli command line string.")
		 (terpri))


      ;; Prepare loc-from:
      (setq loc-from (if (> (string-to-number arg-to) 0)
			 (string-to-number arg-from)
		       (1+
			(-  loc-current-blockheight
			    (string-to-number arg-from)))))

      ;; Prepare loc-to, becomes loc-current-blockheight if arg-from is meant to be "<arg-last-n>":
      (setq loc-to (if (> (string-to-number arg-to) 0)
		       (string-to-number arg-to)
		     loc-current-blockheight))

      ;; Prepare loc-report-progress-p
      (setq loc-report-progress-p (if (string= arg-report-progress "true")
				      200
				    (string-to-number arg-report-progress)))

      ;; Preset loc-verbose-p with nil or t:
      (setq loc-verbose-p (and (stringp arg-verbose-p)
			       (string= arg-verbose-p "true")))

      ;; Calculate loc-number-of-analyzed-blocks
      (setq loc-number-of-analyzed-blocks (1+ (- loc-to loc-from)))

      ;; Display interpretation of command line args
      (princ "------------------------------------")
      (terpri)
      (princ "Interpretation of command line args:")
      (terpri)
      (princ (format "Analyze from %d" loc-from))
      (terpri)
      (princ (format "          to %d (number of blocks to be analyzed: %d)" loc-to loc-number-of-analyzed-blocks))
      (terpri)
      (princ (format "Report progress: %s" (if (> loc-report-progress-p 0) "yes" "no")))
      (terpri)
      (princ (format "Verbose: %s" (if loc-verbose-p "yes" "no")))
      (terpri)
      (princ (format "namecoin-cli string example: %s" (concat arg-namecoin-cli-string " getblockcount")))
      (terpri)
      (princ "------------------------------------")
      (terpri)
      (terpri)

      ;; Start main loop
      (setq loc-results-plist nil)
      (princ (format "Stats from %d to %d (%d blocks):" loc-from loc-to loc-number-of-analyzed-blocks))
      (terpri)
      (sit-for 0) ; for compatibility with all Emacs versions and OS versions
      (loop for i from loc-from to loc-to do

	    (when (and (> loc-report-progress-p 0)
		       (eq 0 (% i loc-report-progress-p)))
	      (princ (format "-------------------%d-------------------" i))
	      (terpri)
	      (sit-for 0))

	    (setq loc-blockhash (shell-command-to-string
				 (concat arg-namecoin-cli-string
					 " getblockhash "
					 (format "%s" i))))
	    (setq loc-blockhash-stripped (car (split-string loc-blockhash)))
	    (setq loc-block (shell-command-to-string
			     (concat arg-namecoin-cli-string
				     " getblock "
				     loc-blockhash-stripped)))

	    ;; The following paragraph needs to be rewritten by making use of json.el.
	    ;; This quick-and-dirty split-string hack should do for the moment:
	    (setq loc-blocksplit-step1 (split-string loc-block "\"coinbase\": \""))
	    (setq loc-blocksplit-step2 (split-string ( cadr loc-blocksplit-step1 ) "\""))
	    (setq loc-hexstring (car loc-blocksplit-step2))
	    (setq loc-coinbaseversion (string-to-number (car (split-string (cadr (split-string loc-block "\"version\":"))))))
	    (setq loc-blocksplit-step3 (split-string loc-block "\"asm\": \"OP_DUP OP_HASH160 "))
	    ;;(when (eq 1 (length loc-blocksplit-step3)) (progn (princ (format "%d: unorthodox asm-OP_HASH160 sequence" i))(terpri)(sit-for 0)))
	    (setq loc-blocksplit-step3 (split-string loc-block "\"asm\":"))
	    (setq loc-blocksplit-step4 (split-string (cadr loc-blocksplit-step3) "OP_HASH160"))
	    (setq loc-hash160 (car (split-string (cadr loc-blocksplit-step4))))

	    (setq loc-txsplit-step0 (split-string loc-block "\"tx\":"))
	    (setq loc-txsplit-step1 (split-string (cadr loc-txsplit-step0) "\\["))
	    (setq loc-txsplit-step2 (split-string (cadr loc-txsplit-step1) "\\]"))
	    (setq loc-txsplit-step3 (split-string (car loc-txsplit-step2) ","))

	    (let (loc-characterlist
		  loc-resultstring
		  loc-resultsymbol
		  loc-tmp-plistmember)

	      ;; convert hex to bin:
	      (setq loc-characterlist (append loc-hexstring nil))
	      (setq loc-resultstring (apply 'string
					    (loop for i from 0 below (length loc-characterlist) by 2 collect
						  (string-to-number (concat (char-to-string (nth i loc-characterlist))
									    (char-to-string (nth (1+ i) loc-characterlist)))
								    16))))

	      ;; Currently we use coinbase strings or previously used BTC hash160 addresses for determining the poolname
	      ;; as https://raw.githubusercontent.com/blockchain/Blockchain-Known-Pools/master/pools.json
	      ;; is not accurate enough at the moment.
	      (cond  ; We are going to use the "intern" function instead of make-symbol or make-atom (for compatibility with Emacs22).
	       ((string= "e4b883e5bda9" (substring loc-hexstring 8 (+ 8 (min 12 (length loc-hexstring)))))
		(setq loc-resultsymbol (intern (concat "DiscusFish-" (format "%05x" loc-coinbaseversion)))))
	       ((string= "c825a1ecf2a6830c4401620c3a16f1995057c2ab" loc-hash160)
		(setq loc-resultsymbol (intern (concat "F2Pool-"  (format "%05x" loc-coinbaseversion)))))
	       ((string= "80ad90d403581fa3bf46086a91b2d9d4125db6c1" loc-hash160)
		(setq loc-resultsymbol (intern (concat "ghash.io-"  (format "%05x" loc-coinbaseversion)))))
	       ((string-match "slush" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "slush-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "Eligius" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "Eligius-" (format "%05x" loc-coinbaseversion)))))

	       ((string= "2bf64e2be04ba49a87d17ae9a9009d72efbc4724" loc-hash160)
		(setq loc-resultsymbol (intern (concat "myBTCcoin-" (format "%05x" loc-coinbaseversion)))))
               ((string= "cef3550ff9e637ddd120717d43fc21f8a563caf8" loc-hash160)
                (setq loc-resultsymbol (intern (concat "Bixin-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "bitparking" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "bitparking-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "mmpool" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "mmpool-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "bitminter" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "bitminter-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "AntPool" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "Antpool-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "TangPool" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "TangPool-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "give-me-coins" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "give-me-coins-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "TBDice" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "TBDice-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "huobi" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "huobi-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "Guild" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "BTC_Guild-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "/BTCC/" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "BTCC-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "/mined by gbminers/" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "GBMiners-" (format "%05x" loc-coinbaseversion)))))
	       ((string= "2c30a6aaac6d96687291475d7d52f4b469f665a6" loc-hash160)
		(setq loc-resultsymbol (intern (concat "BTCC-" (format "%05x" loc-coinbaseversion)))))
	       ((string= "adbc0b208ba2c41fbe9599942cba3c205fe37ed7" loc-hash160)
		(setq loc-resultsymbol (intern (concat "p2p-1GqdAgC1zmzLFMg3XyYctSECWUjkbT7Nv9-"  (format "%05x" loc-coinbaseversion)))))
	       ((string= "20dfa394c831d087c5695188410b15bda888d903" loc-hash160)
		(setq loc-resultsymbol (intern (concat "p2p-13zpXrBUF4nYoqBvT2RdbJiNUxH5PBuP74-"  (format "%05x" loc-coinbaseversion)))))
	       ((string= "24e469e9b8c9b3487378717d357c19f8ed062fa7" loc-hash160)
		(setq loc-resultsymbol (intern (concat "p2p-14N4xNSDtNYatfCgHhyRaAhdZtaczUBnAa-" (format "%05x" loc-coinbaseversion)))))
	       ((string= "159bb0eb02cb1c35f5d4942420d23fff8b6ae62d" loc-hash160)
		(setq loc-resultsymbol (intern (concat "p2p-12yFhdpG3TxASryDGzWZ268zWQ3hcj2JNu-" (format "%05x" loc-coinbaseversion)))))
	       ((string= "ed3104267d771aeaf7fe9a7fd5dc56f2dc18ad8f" loc-hash160)
		(setq loc-resultsymbol (intern (concat "p2p-1Nd9sArhgzgLwhaYqoi7Pnm6Jcv5Z58Ghm-" (format "%05x" loc-coinbaseversion)))))
	       ((string= "298d57f3f98fe1121dafcee43bf8cdfed11f2444" loc-hash160)
		(setq loc-resultsymbol (intern (concat "p2p-14ni2b7mEnvAoXYf6hzxv7wUFAt1TCoavq-" (format "%05x" loc-coinbaseversion)))))
	       ((string= "df5b7a824ee01515c8d8a2bf6813a93aeef10b36" loc-hash160)
		(setq loc-resultsymbol (intern (concat "p2p-1MN1GpXDADiG1ioGbgz8Tyim2MeBcTrb38-" (format "%05x" loc-coinbaseversion)))))

	       ((string= "27e4efc33ade08c78b85e1bb3931f696271277aa" loc-hash160)
		(setq loc-resultsymbol (intern (concat "multipool.us-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "StratumPool" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "StratumPool-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "nodeStratum" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "nodeStratum-" (format "%05x" loc-coinbaseversion)))))

	       ((string-match "EMC" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "EclipseMC-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "/ViaBTC/" loc-resultstring)
                (setq loc-resultsymbol (intern (concat "ViaBTC-" (format "%05x" loc-coinbaseversion)))))
               ((string-match "/BTC.COM/" loc-resultstring)
                (setq loc-resultsymbol (intern (concat "BTC.com-" (format "%05x" loc-coinbaseversion)))))
               ((string-match "/BTC.TOP/" loc-resultstring)
                (setq loc-resultsymbol (intern (concat "BTC.TOP-" (format "%05x" loc-coinbaseversion)))))
               ((string-match "/BitClub Network/" loc-resultstring)
                (setq loc-resultsymbol (intern (concat "BitClub Network-" (format "%05x" loc-coinbaseversion)))))
	       ((string-match "P2SH" loc-resultstring)
		(setq loc-resultsymbol (intern (concat "unknown-P2SH-supporter-" (format "%05x" loc-coinbaseversion)))))
	       (t
		(setq loc-resultsymbol (intern (concat "unknown-" (format "%05x" loc-coinbaseversion))))))

	      ;; multipool.us and StratumPool seem to be identical. Funds end up at the same NMC address:
	      ;; "mulitpool.us"
	      ;; 27e4efc33ade08c78b85e1bb3931f696271277aa
	      ;; 14dwcGrTn9vwSwSkK4rZUJnRSUCswB737X
	      ;; N7t78VdgFnnbFUo3HNtrVeBevSbEuiLwRx ---> N3S9cBZiB6L9SdtiiZGBynyseD6yjG8Qc9
	      ;; "StratumPool"
	      ;; e2822d8308664818d6b07790451cf1cace2afe92
	      ;; 1MeffGLauEj2CZ18hRQqUauTXb9JAuLbGw = multipool.us according to github.
	      ;; See btc tx cd9c38b55598fe73afeffc80102a7d03ebe522e9eb6bab7088933032877baa4d
	      ;; N7rv47zEcxB8C71YvWBVGkkYjCNmGRXC9v ---> N3S9cBZiB6L9SdtiiZGBynyseD6yjG8Qc9

	      ;; Here is another hack: display the last -n meaningful bytes of the coinbase string,
	      ;; derived empirically from recent pool admins's preferences:
	      (when loc-verbose-p
		(setq loc-meaningful (cond ((eq loc-resultsymbol 'DiscusFish-10103)
					    (substring loc-resultstring -41))
					   ((eq loc-resultsymbol 'DiscusFish-10104)
					    (substring loc-resultstring -41))
					   ((eq loc-resultsymbol 'ghash.io-10103)
					    (substring loc-resultstring -46))
					   ((eq loc-resultsymbol 'Eligius-10103)
					    (substring loc-resultstring -16))
					   ((eq loc-resultsymbol 'Eligius-10104)
					    (substring loc-resultstring -16))
					   ((eq loc-resultsymbol 'EclipseMC-10103)
					    (substring loc-resultstring -17))
					   ((eq loc-resultsymbol 'EclipseMC-10104)
					    (substring loc-resultstring -17))
					   ((eq loc-resultsymbol 'slush-10103)
					    (substring loc-resultstring -7))
					   ((eq loc-resultsymbol 'bitminter-10103)
					    (substring loc-resultstring -17))
					   ((eq loc-resultsymbol 'mmpool-10103)
					    (substring loc-resultstring -16))
					   (t
					    loc-resultstring)))
		(setq loc-meaningful (loop for i across loc-meaningful concat (if (and (> i #x20) (< i #x7f))
										  (string i)
										"."))) ; remove non-ascii chars
		(princ (format "%6d %s %4d %20s %s" i loc-hash160 (length loc-txsplit-step3) loc-resultsymbol
			       loc-meaningful))
		(terpri)
		(sit-for 0))

	      (if (setq loc-tmp-plistmember (plist-member loc-results-plist loc-resultsymbol))
		  (incf (cadr loc-tmp-plistmember))
		(setq loc-results-plist (plist-put loc-results-plist loc-resultsymbol 1)))))
      (terpri)

      ;; A property list simplyfies collecting the data, whereas sort works only an assoc lists.
      ;; Convert the plist to an alist:
      (setq loc-results-alist (loop for i in loc-results-plist by 'cddr
				    for j from 1 by 2
				    collect
				    (cons i (elt loc-results-plist j))))

      (setq loc-results-alist (sort loc-results-alist (lambda (a b) (> (cdr a)(cdr b)))))

      (princ (format "  Total     %%  Pool or Miner, Blockversion"))
      (terpri)
      (loop for i in loc-results-alist do
	    (princ (format "%7d %5.1f  %s"
			   (cdr i)
			   (/ (* 100.0 (cdr i)) loc-number-of-analyzed-blocks)
			   (car i)))
	    (terpri))

      (terpri))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END defun mm-blockstats-main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(byte-compile 'mm-blockstats-main) ; Compiling reduces processing time by only 5 %. The main bottleneck here is RPC.

;;;;;;;;;;;;;;;;;;;;;
;;; Run:
;;;;;;;;;;;;;;;;;;;;;
(if (= 5 (length command-line-args-left))
    (mm-blockstats-main (elt command-line-args-left 0)
			(elt command-line-args-left 1)
			(elt command-line-args-left 2)
			(elt command-line-args-left 3)
			(elt command-line-args-left 4))
  (print "Error: wrong number of arguments.")))


;;; Alternatively, start within an Emacs window with Cntl-x Cntl-e after applying the args directly:
;;;  (mm-blockstats-main <from-blockheight> <to-blockheight> <report-progress> <verbose> <namecoin-cli-string>))
;;; In this case all arguments have to be strings (for compatibility with command line invokation).
;;; Note the extra bracket terminating the progn statement. This way both the defun and the starting of mm-blockstats-main happen in one go,
;;; e.g.
;;;  (mm-blockstats-main "260000"   ;<from-blockheight>
;;;		         "270000"   ;<to-blockheight>
;;;		         "500"      ;<report-progress> every n lines
;;;		         "false"    ;<verbose>
;;;		         "./namecoin-cli -datadir=. -conf=./namecoin.conf"))
;;; or
;;;
;;;  (mm-blockstats-main "100"      ;<last-n-blocks>
;;;		         "0"        ;"0" therefore the first parameter means <last-n-blocks> instead of <from-blockheight>
;;;		         "false"    ;<report-progress>
;;;		         "true"     ;<verbose>
;;;		         "./namecoin-cli -datadir=. -conf=./namecoin.conf"))
