;;********************************************
;; Lisp code for an NCL major mode
;;********************************************
;; September 20 2011
;; Revision 0.34
;;  - Updated to include new functions, resources, and
;;    keywords added in NCL 6.0.0
;;
;; Revision 0.33
;; Changes to 0.32 by T. Corti, ETH Zurich and David Brown,
;; Changes between 0.2 and 0.3 by C. Schreck and A. Srock, University at Albany
;; Changes between 0.1 and 0.2 Provided by Heiko Klein of Norway

;; August 19 2003 Sylvia Murphy
;; National Center for Atmospheric Research
;; Does text highlighting for NCL reserved words, built-in functions,
;; gsn* functions, contributed and shea-util functions, text, and comments.
;; Does automatic indenting between begin and end statments, and within
;; do loops and if statements.
;;
;; Emacs has a lot more power that these functions. I do not use that
;; functionality, so i did not spend any more time trying to add abbreviations,
;; special keymaps etc.
;;
;; Updates in version 0.32
;; Added Comment Handling (M-;).
;;  - Insert a comment at the end of the current line
;;  - Alternatively comment/uncomment selected region
;; Use syntactic fontification for comments and strings
;; Correct fontification of strings containing a semicolon (;)
;; Added highlightning for resources using font-lock-constant-face
;; All documented functions are now highlighted (modification by D. Brown)
;;
;; Updates in version 0.3:
;; Added more keywords (full list from NCL documentation)
;; Changed color mapping (font-lock) settings:
;;   - removed usage of font-lock-reference-face
;;   - added usage of font-lock-builtin-face
;;   - NCL built-in functions now use font-lock-builtin-face
;;   - contributed and shea_util functions now use font-lock-function-face
;;   - added boolean and value test keywords
;;   - added keywords for beginning and ending arrays: (/ and /)
;;   - all keywords now use font-lock-keyword-face
;;   - explicitly fontifies strings with font-lock-string-face
;; Changed syntax type of underscore to "word" instead of punctuation
;; Updated "How to Use" instructions for ease of inclusion with Xemacs
;;
;; KNOWN PROBLEMS in version 0.32:
;; 1) Comment Handling does not work in xemacs
;; 2) Comments may not fontify on file open in xemacs
;;
;; KNOWN PROBLEMS in version 0.3:
;; 1) Comments with embedded strings don't initially fontify properly, but
;;    do change if line modified somehow
;; 2) Strings containing a semicolon (;) do not fontify properly
;;
;; KNOWN PROBLEMS THAT VERSION 0.2 fixed
;; 1) Works with xemacs 21.*
;; 2) Works with emacs 20.*

;; KNOWN PROBLEMS in Version 0.1
;; 1) Only partially works with emacs version 20.3.2
;;    a) highlights only comments and text, and only after tabs
;;    b) indentation appears to work
;; 2) Does not work with xemacs
;; 3) Not all NCL built-in functions are highlighted. I listed MY favorite
;;    ones.
;; 4) Have not demonstrated how to change the indentation value in .emacs
;; 5) The ncl-in-comment function does not work. Its calls are commented out.
;;
;;********************************************
;; HOW TO USE
;;********************************************
;; 1) place this file somewhere on your local system e.g. ~your_home/bin

;; 2) in your .emacs or .xemacs/custom.el file, add and properly modify //
;; the following (without the comments):
;; (setq auto-mode-alist (cons '("\.ncl$" . ncl-mode) auto-mode-alist))
;; (autoload 'ncl-mode "LOCATION/ncl.el")
;; (add-hook 'ncl-mode-hook
;;           (lambda ()
;;             )
;;           )

;; 3) setup display colors for font-lock.  You may also want to set default
;; foreground and background colors.  Colors can be Xwindows names or #rrggbb.
;; These should also go somewhere in your .emacs or .xemacs/custom.el file.
;;     ; highlight comments
;;         (set-face-foreground font-lock-comment-face "FireBrick")
;;     ; highlight strings
;;         (set-face-foreground font-lock-string-face "Salmon")
;;     ; highlight keywords, array descriptors, and tests
;;         (set-face-foreground font-lock-keyword-face "Purple")
;;     ; highlight built-in functions
;;         (set-face-foreground font-lock-builtin-face "Blue")
;;     ; highlight gsn* functions
;;         (set-face-foreground font-lock-variable-name-face "SteelBlue")
;;     ; highlight shea_util and contributed functions
;;         (set-face-foreground font-lock-function-name-face  "CadetBlue")
;;     ; highlight resources
;;         (set-face-foreground font-lock-constant-face  "ForestGreen")
;;;

;;; code starts here
;;=================================================================
;; user options
;;=================================================================
(defgroup ncl nil
  "major mode to edit Ncar Command Line(NCL) language "
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom ncl-mode-hook nil
  "Hook run when entering NCL mode."
  :type    'hook
  ;; Not the only safe options, but some common ones.
  :safe    (lambda (value) (member value '((ncl-add-imenu-menu) nil)))
  :options '(ncl-add-imenu-menu)
  :group   'ncl)

;;;; COOKIE: STARTS HERE =DO NOT DELETE=

(defvar ncl-keywords
  '(
    "begin" "break" "byte" "character" "continue" "create" "defaultapp" "do" "double" "else" "end" "enumeric" "external" "file" "float" "function" "getvalues" "graphic" "group" "if" "integer" "int64" "list" "load" "local" "logical" "long" "new" "_Missing" "Missing" "new" "noparent" "numeric" "procedure" "quit" "QUIT" "Quit" "record" "return" "setvalues" "short" "snumeric" "stop" "string" "then" "ubyte" "uint" "uint64" "ulong" "ushort" "while"
    ) "Reserved Keywords in ncl")


(defvar ncl-key-operators
  '(
    "(/" "/)" "\\" ".eq." ".ne." ".lt." ".le." ".gt." ".ge." ".and." ".or." ".not." ".xor."
    ) "Operators in NCL")


(defvar ncl-key-builtin
  '(
    "abs" "acos" "addfile" "addfiles" "all" "angmom_atm" "any" "area_conserve_remap" "area_hi2lores" "asciiread" "asciiwrite" "asin" "atan" "atan2" "attsetvalues" "avg" "betainc" "bin_avg" "bin_sum" "cancor" "cbinread" "cbinwrite" "cd_calendar" "cd_inv_calendar" "cdfbin_p" "cdfbin_pr" "cdfbin_s" "cdfbin_xn" "cdfchi_p" "cdfchi_x" "cdfgam_p" "cdfgam_x" "cdfnor_p" "cdfnor_x" "cdft_p" "cdft_t" "ceil" "center_finite_diff" "center_finite_diff_n" "cfftb" "cfftf" "cfftf_frq_reorder" "charactertodouble" "charactertofloat" "charactertointeger" "charactertolong" "charactertoshort" "charactertostring" "chartodouble" "chartofloat" "chartoint" "chartointeger" "chartolong" "chartoshort" "chartostring" "chiinv" "clear" "conform" "conform_dims" "cos" "cosh" "covcorm" "craybinnumrec" "craybinrecread" "csa1" "csa1d" "csa1s" "csa1x" "csa1xd" "csa1xs" "csa2" "csa2d" "csa2l" "csa2ld" "csa2ls" "csa2lx" "csa2lxd" "csa2lxs" "csa2s" "csa2x" "csa2xd" "csa2xs" "csa3" "csa3d" "csa3l" "csa3ld" "csa3ls" "csa3lx" "csa3lxd" "csa3lxs" "csa3s" "csa3x" "csa3xd" "csa3xs" "csc2s" "csgetp" "css2c" "cssetp" "cssgrid" "csstri" "csvoro" "cumsum" "cz2ccm" "datatondc" "day_of_week" "day_of_year" "days_in_month" "default_fillvalue" "delete" "destroy" "dewtemp_trh" "dim_avg" "dim_avg_n" "dim_avg_wgt" "dim_avg_wgt_n" "dim_cumsum" "dim_cumsum_n" "dim_gbits" "dim_max" "dim_max_n" "dim_median" "dim_median_n" "dim_min" "dim_min_n" "dim_num" "dim_num_n" "dim_numrun_n" "dim_pqsort" "dim_pqsort_n" "dim_product" "dim_product_n" "dim_rmsd" "dim_rmsd_n" "dim_rmvmean" "dim_rmvmean_n" "dim_rmvmed" "dim_rmvmed_n" "dim_standardize" "dim_standardize_n" "dim_stat4" "dim_stat4_n" "dim_stddev" "dim_stddev_n" "dim_sum" "dim_sum_n" "dim_sum_wgt" "dim_sum_wgt_n" "dim_variance" "dim_variance_n" "dimsizes" "doubletobyte" "doubletochar" "doubletocharacter" "doubletofloat" "doubletoint" "doubletointeger" "doubletolong" "doubletoshort" "dpres_hybrid_ccm" "dpres_plevel" "draw" "dsgetp" "dsgrid2" "dsgrid2d" "dsgrid2s" "dsgrid3" "dsgrid3d" "dsgrid3s" "dspnt2" "dspnt2d" "dspnt2s" "dspnt3" "dspnt3d" "dspnt3s" "dssetp" "dtrend" "dtrend_msg" "dtrend_msg_n" "dtrend_n" "dtrend_quadratic" "dtrend_quadratic_msg_n" "dv2uvF" "dv2uvf" "dv2uvG" "dv2uvg" "dz_height" "echo_off" "echo_on" "eof2data" "eof_varimax" "eofcor" "eofcor_pcmsg" "eofcor_ts" "eofcov" "eofcov_pcmsg" "eofcov_ts" "eofunc" "eofunc_ts" "eofunc_varimax" "equiv_sample_size"
    "erf" "erfc" "esacr" "esacv" "esccr" "esccv" "escorc" "escovc" "exit" "exp" "exp_tapersh" "exp_tapersh_wgts" "exp_tapershC" "ezfftb" "ezfftf" "f2fosh" "f2foshv" "f2fsh" "f2fshv" "f2gsh" "f2gshv" "fabs" "fbindirread" "fbindirwrite" "fbinnumrec" "fbinread" "fbinrecread" "fbinrecwrite" "fbinwrite" "fft2db" "fft2df" "fileattdef" "filechunkdimdef" "filedimdef" "filevarattdef" "filevarchunkdef" "filevarcompressleveldef" "filevardef" "filevardimsizes" "filwgts_lancos" "filwgts_lanczos" "filwgts_normal" "floattobyte" "floattochar" "floattocharacter" "floattoint" "floattointeger" "floattolong" "floattoshort" "floor" "fluxEddy" "fo2fsh" "fo2fshv" "fourier_info" "frame" "fspan" "ftcurv" "ftcurvd" "ftcurvi" "ftcurvp" "ftcurvpi" "ftcurvps" "ftcurvs" "ftest" "ftgetp" "ftkurv" "ftkurvd" "ftkurvp" "ftkurvpd" "ftsetp" "ftsurf" "g2fsh" "g2fshv" "g2gsh" "g2gshv" "gamma" "gammainc" "gaus" "gaus_lobat" "gaus_lobat_wgt" "gc_aangle" "gc_clkwise" "gc_dangle" "gc_inout" "gc_latlon" "gc_onarc" "gc_pnt2gc" "gc_qarea" "gc_tarea" "generate_2d_array" "get_cpu_time" "get_ncl_version" "get_script_name" "get_script_prefix_name" "get_sphere_radius" "getbitsone" "getenv" "getfiledimsizes" "getfilevaratts" "getfilevardims" "getfilevardimsizes" "getfilevarnames" "getfilevartypes" "getvaratts" "getvardims" "gradsf" "gradsg" "greg2jul" "grid2triple" "hlsrgb" "hsvrgb" "hydro" "hyi2hyo" "idsfft" "igradsf" "igradsF" "igradsg" "igradsG" "ilapsf" "ilapsF" "ilapsg" "ilapsG" "ilapvf" "ilapvg" "ind" "ind_resolve" "int2p" "int2p_n" "integertobyte" "integertochar" "integertocharacter" "integertoshort" "inttobyte" "inttochar" "inttoshort" "inverse_matrix" "isatt" "isbigendian" "isbyte" "ischar" "iscoord" "isdefined" "isdim" "isdimnamed" "isdouble" "isenumeric" "isfile" "isfilepresent" "isfilevar" "isfilevaratt" "isfilevarcoord" "isfilevardim" "isfloat" "isfunc" "isgraphic" "isint" "isint64" "isinteger" "isleapyear" "islogical" "islong" "ismissing" "isnan_ieee" "isnumeric" "ispan" "isproc" "isshort" "issnumeric" "isstring" "isubyte" "isuint" "isuint64" "isulong" "isunlimited" "isunsigned" "isushort" "isvar" "jul2greg" "kron_product" "lapsF" "lapsf" "lapsG" "lapsg" "lapvf" "lapvg" "latlon2utm" "lclvl" "lderuvf" "lderuvg" "linint1" "linint1_n" "linint2" "linint2_points" "linmsg" "linmsg_n" "linrood_latwgt" "linrood_wgt" "list_files" "list_filevars" "list_hlus"
    "list_procfuncs" "list_vars" "ListGetType" "ListSetType" "loadscript" "local_max" "local_min" "log" "log10" "longtobyte" "longtochar" "longtocharacter" "longtoint" "longtointeger" "longtoshort" "lspoly" "mask" "max" "maxind" "min" "minind" "mixhum_ptd" "mixhum_ptrh" "mjo_cross_coh2pha" "mjo_cross_segment" "moc_globe_atl" "monthday" "natgrid" "natgridd" "natgrids" "ncargpath" "ncargversion" "ndctodata" "ndtooned" "new" "ngezlogo" "nggcog" "nggetp" "nglogo" "ngsetp" "NhlAddAnnotation" "NhlAddData" "NhlAddOverlay" "NhlAddPrimitive" "NhlAppGetDefaultParentId" "NhlChangeWorkstation" "NhlClassName" "NhlClearWorkstation" "NhlDataPolygon" "NhlDataPolyline" "NhlDataPolymarker" "NhlDataToNDC" "NhlDestroy" "NhlDraw" "NhlFrame" "NhlFreeColor" "NhlGetBB" "NhlGetClassResources" "NhlGetErrorObjectId" "NhlGetNamedColorIndex" "NhlGetParentId" "NhlGetParentWorkstation" "NhlGetWorkspaceObjectId" "NhlIsAllocatedColor" "NhlIsApp" "NhlIsDataComm" "NhlIsDataItem" "NhlIsDataSpec" "NhlIsTransform" "NhlIsView" "NhlIsWorkstation" "NhlName" "NhlNDCPolygon" "NhlNDCPolyline" "NhlNDCPolymarker" "NhlNDCToData" "NhlNewColor" "NhlNewDashPattern" "NhlNewMarker" "NhlPalGetDefined" "NhlRemoveAnnotation" "NhlRemoveData" "NhlRemoveOverlay" "NhlRemovePrimitive" "NhlSetColor" "NhlSetDashPattern" "NhlSetMarker" "NhlUpdateData" "NhlUpdateWorkstation" "nice_mnmxintvl" "nngetaspectd" "nngetaspects" "nngetp" "nngetsloped" "nngetslopes" "nngetwts" "nngetwtsd" "nnpnt" "nnpntd" "nnpntend" "nnpntendd" "nnpntinit" "nnpntinitd" "nnpntinits" "nnpnts" "nnsetp" "num" "obj_anal_ic" "omega_ccm" "onedtond" "overlay" "paleo_outline" "pdfxy_bin" "poisson_grid_fill" "pop_remap" "prcwater_dp" "pres2hybrid" "pres_hybrid_ccm" "pres_sigma" "print" "printFileVarSummary" "printVarSummary" "product" "pslec" "pslhor" "pslhyp" "qsort" "rand" "random_chi" "random_gamma" "random_normal" "random_setallseed" "random_uniform" "rcm2points" "rcm2rgrid" "rdsstoi" "reg_multlin" "regcoef" "regCoef" "regline" "relhum" "replace_ieeenan" "reshape" "rgbhls" "rgbhsv" "rgbyiq" "rgrid2rcm" "rhomb_trunc" "rhomb_trunC" "rip_cape_2d" "rip_cape_3d" "round" "rtest" "runave" "runave_n" "set_default_fillvalue" "set_sphere_radius" "setfileoption" "sfvp2uvf" "sfvp2uvg" "shaeC" "shaec" "shagC" "shagc" "shgetnp" "shgetp" "shgrid" "shorttobyte" "shorttochar" "shorttocharacter" "show_ascii" "shsec" "shseC" "shsetp" "shsgC" "shsgc" "shsgc_R42" "sigma2hybrid" "simpeq" "simpne" "sin" "sindex_yrmo" "sinh" "sizeof" "sleep" "smth9" "snindex_yrmo" "solve_linsys" "sparse_matrix_mult" "spcorr" "spcorr_n" "specx_anal" "specxy_anal" "sprintf" "sprinti" "sqrt" "sqsort" "srand" "stat2" "stat4"
    "stat_medrng" "stat_trim" "status_exit" "stdatmus_p2tdz" "stdatmus_z2tdp" "stddev" "str_capital" "str_concat" "str_fields_count" "str_get_cols" "str_get_dq" "str_get_field" "str_get_nl" "str_index_of_substr" "str_insert" "str_is_blank" "str_join" "str_left_strip" "str_lower" "str_match" "str_match_ic" "str_match_ind" "str_match_ind_ic" "str_right_strip" "str_split" "str_split_by_length" "str_split_csv" "str_squeeze" "str_strip" "str_sub_str" "str_switch" "str_upper" "stringtochar" "stringtocharacter" "stringtodouble" "stringtofloat" "stringtoint" "stringtointeger" "stringtolong" "stringtoshort" "strlen" "student_t" "sum" "svd_lapack" "svdcov" "svdcov_sv" "svdstd" "svdstd_sv" "system" "systemfunc" "tan" "tanh" "taper" "taper_n" "tdclrs" "tdctri" "tdcudp" "tdcurv" "tddtri" "tdez2d" "tdez3d" "tdgetp" "tdgrds" "tdgrid" "tdgtrs" "tdinit" "tditri" "tdlbla" "tdlblp" "tdlbls" "tdline" "tdlndp" "tdlnpa" "tdlpdp" "tdmtri" "tdotri" "tdpara" "tdplch" "tdprpa" "tdprpi" "tdprpt" "tdsetp" "tdsort" "tdstri" "tdstrs" "tdttri" "tobyte" "tochar" "todouble" "tofloat" "toint" "toint64" "tointeger" "tolong" "toshort" "tosigned" "tostring" "toubyte" "touint" "touint64" "toulong" "tounsigned" "toushort" "tri_trunC" "tri_trunc" "triple2grid" "triple2grid2d" "trop_wmo" "ttest" "typeof" "undef" "unique_string" "update" "ushorttoint" "ut_calendar" "ut_inv_calendar" "utm2latlon" "uv2dv_cfd" "uv2dvf" "uv2dvF" "uv2dvg" "uv2dvG" "uv2sfvpF" "uv2sfvpf" "uv2sfvpG" "uv2sfvpg" "uv2vr_cfd" "uv2vrdvF" "uv2vrdvf" "uv2vrdvG" "uv2vrdvg" "uv2vrF" "uv2vrf" "uv2vrG" "uv2vrg" "v5d_close" "v5d_create" "v5d_setLowLev" "v5d_setUnits" "v5d_write" "v5d_write_var" "variance" "vhaeC" "vhaec" "vhagC" "vhagc" "vhseC" "vhsec" "vhsgc" "vhsgC" "vibeta" "vinth2p" "vinth2p_ecmwf" "vinth2p_ecmwf_nodes" "vinth2p_nodes" "vintp2p_ecmwf" "vr2uvf" "vr2uvF" "vr2uvg" "vr2uvG" "vrdv2uvf" "vrdv2uvF" "vrdv2uvg" "vrdv2uvG" "wavelet" "wavelet_default" "wgt_areaave" "wgt_areaave2" "wgt_arearmse" "wgt_arearmse2" "wgt_areasum2" "wgt_runave" "wgt_runave_n" "wgt_vert_avg_beta" "wgt_volave" "wgt_volave_ccm" "wgt_volrmse" "wgt_volrmse_ccm" "where" "wk_smooth121" "wmbarb" "wmbarbmap" "wmdrft" "wmgetp" "wmlabs" "wmsetp" "wmstnm" "wmvect" "wmvectmap" "wmvlbl" "wrf_avo" "wrf_cape_2d" "wrf_cape_3d" "wrf_dbz" "wrf_eth" "wrf_helicity" "wrf_ij_to_ll" "wrf_interp_1d" "wrf_interp_2d_xy" "wrf_interp_3d_z"
    "wrf_latlon_to_ij" "wrf_ll_to_ij" "wrf_pvo" "wrf_rh" "wrf_slp" "wrf_smooth_2d" "wrf_td" "wrf_tk" "wrf_updraft_helicity" "wrf_uvmet" "write_matrix" "yiqrgb" "z2geouv" "zonal_mpsi"
    ) "ncl built-in functions")


(defvar ncl-key-contrib
  '(
    "addfiles_GetVar" "area_conserve_remap_Wrap" "area_hi2lores_Wrap" "array_append_record" "assignFillValue" "byte2flt" "calcDayAnomTLL" "calcMonAnomLLLT" "calcMonAnomLLT" "calcMonAnomTLL" "calcMonAnomTLLL" "changeCase" "changeCaseChar" "clmDayTLL" "clmDayTLLL" "clmMon2clmDay" "clmMonLLLT" "clmMonLLT" "clmMonTLL" "clmMonTLLL" "closest_val" "copy_VarAtts" "copy_VarCoords" "copy_VarCoords_1" "copy_VarCoords_2" "copy_VarMeta" "copyatt" "crossp3" "cshstringtolist" "cssgrid_Wrap" "dble2flt" "decimalPlaces" "delete_VarAtts" "dim_avg_n_Wrap" "dim_avg_wgt_n_Wrap" "dim_avg_wgt_Wrap" "dim_avg_Wrap" "dim_cumsum_n_Wrap" "dim_cumsum_Wrap" "dim_max_n_Wrap" "dim_min_n_Wrap" "dim_rmsd_n_Wrap" "dim_rmsd_Wrap" "dim_rmvmean_n_Wrap" "dim_rmvmean_Wrap" "dim_rmvmed_n_Wrap" "dim_rmvmed_Wrap" "dim_standardize_n_Wrap" "dim_standardize_Wrap" "dim_stddev_n_Wrap" "dim_stddev_Wrap" "dim_sum_n_Wrap" "dim_sum_wgt_n_Wrap" "dim_sum_wgt_Wrap" "dim_sum_Wrap" "dim_variance_n_Wrap" "dim_variance_Wrap" "dpres_plevel_Wrap" "dtrend_leftdim" "dv2uvF_Wrap" "dv2uvG_Wrap" "eofcor_Wrap" "eofcov_Wrap" "eofunc_ts_Wrap" "eofunc_varimax_reorder" "eofunc_varimax_Wrap" "eofunc_Wrap" "epsZero" "f2fosh_Wrap" "f2foshv_Wrap" "f2fsh_Wrap" "f2fshv_Wrap" "f2gsh_Wrap" "f2gshv_Wrap" "fbindirSwap" "fbinseqSwap1" "fbinseqSwap2" "flt2dble" "flt2string" "fo2fsh_Wrap" "fo2fshv_Wrap" "g2fsh_Wrap" "g2fshv_Wrap" "g2gsh_Wrap" "g2gshv_Wrap" "generate_unique_indices" "genNormalDist" "get1Dindex" "get1Dindex_Collapse" "get1Dindex_Exclude" "get_file_suffix" "GetFillColor" "GetFillColorIndex" "getFillValue" "getind_latlon2d" "getVarDimNames" "getVarFillValue" "grib_stime2itime" "hyi2hyo_Wrap" "ilapsF_Wrap" "ilapsG_Wrap" "ind_nearest_coord" "indStrSubset" "int2dble" "int2flt" "int2p_n_Wrap" "int2p_Wrap" "isMonotonic" "isStrSubset" "latGau" "latGauWgt" "latGlobeF" "latGlobeFo" "latRegWgt" "linint1_n_Wrap" "linint1_Wrap" "linint2_points_Wrap" "linint2_Wrap" "local_max_1d" "local_min_1d" "lonFlip" "lonGlobeF" "lonGlobeFo" "lonPivot" "merge_levels_sfc" "mod" "month_to_annual" "month_to_annual_weighted" "month_to_season" "month_to_season12" "month_to_seasonN" "monthly_total_to_daily_mean" "nameDim" "natgrid_Wrap" "NewCosWeight" "niceLatLon2D" "NormCosWgtGlobe" "numAsciiCol" "numAsciiRow" "numeric2int" "obj_anal_ic_deprecated" "obj_anal_ic_Wrap" "omega_ccm_driver" "oneDtostring" "pack_values" "pattern_cor" "pdfx" "pdfxy" "pres2hybrid_Wrap" "printMinMax" "quadroots" "rcm2points_Wrap" "rcm2rgrid_Wrap" "readAsciiHead" "readAsciiTable" "region_ind" "relhum_ttd" "remap_elements" "replaceSingleChar" "RGBtoCmap" "rgrid2rcm_Wrap" "rho_mwjf" "rm_single_dims" "rmAnnCycle1D" "rmInsufData" "rmMonAnnCycLLLT" "rmMonAnnCycLLT" "rmMonAnnCycTLL" "runave_n_Wrap" "runave_Wrap" "short2flt" "short2flt_hdf" "shsgc_R42_Wrap" "smth9_Wrap" "smthClmDayTLL" "SqrtCosWeight" "stat_dispersion" "stdMonLLLT" "stdMonLLT" "stdMonTLL" "stdMonTLLL" "symMinMaxPlt" "table_attach_columns" "table_attach_rows" "time_to_newtime" "transpose" "triple2grid_Wrap" "ut_convert" "uv2dvF_Wrap" "uv2dvG_Wrap" "uv2vrF_Wrap" "uv2vrG_Wrap" "vr2uvF_Wrap" "vr2uvG_Wrap" "wallClockElapseTime" "wave_number_spc" "wgt_areaave_Wrap" "wgt_runave_leftdim" "wgt_runave_n_Wrap" "wgt_runave_Wrap"
    "yyyyddd_to_yyyymmdd" "yyyymm_time" "yyyymm_to_yyyyfrac" "yyyymmdd_time" "yyyymmdd_to_yyyyddd" "yyyymmdd_to_yyyyfrac" "yyyymmddhh_to_yyyyfrac" "zonal_mpsi_Wrap" "zonalAve"
    ) "contributed functions")


(defvar ncl-key-diag
  '(
    "band_pass_area_time" "band_pass_area_time_plot" "band_pass_hovmueller" "band_pass_hovmueller_plot" "band_pass_latlon_time" "band_pass_latlon_time_plot" "decomposeSymAsym" "mjo_cross" "mjo_cross_plot" "mjo_phase_background" "mjo_space_time_cross" "mjo_spectra" "mjo_spectra_season" "mjo_wavenum_freq_season" "mjo_wavenum_freq_season_plot" "mjo_xcor_lag_ovly" "mjo_xcor_lag_ovly_panel" "mjo_xcor_lag_season" "resolveWavesHayashi" "wkSpaceTime" "wkSpaceTime_cam"
    ) "diagnostics functions")


(defvar ncl-key-pop
  '(
    "PopLatLon" "PopLatLonV"
    ) "pop_remap functions")


(defvar ncl-key-shea
  '(
    "add90LatX" "add90LatY" "boxplot" "ColorNegDashZeroPosContour" "ColorShadeLeGeContour" "drawNDCGrid" "infoTimeStamp" "landsea_mask" "msgValOutline" "pie_chart" "plt_pdfxy" "setColorContourClear" "ShadeCOI" "ShadeGeLeContour" "ShadeGtContour" "ShadeLtContour" "ShadeLtGtContour" "specx_ci"
    ) "shea_util functions")


(defvar ncl-key-skewt
  '(
    "skewT_BackGround" "skewT_PlotData"
    ) "skewt functions")


(defvar ncl-key-user
  '(
    "calendar_decode2" "kf_filter" "run_cor" "time_axis_labels" "ut_string"
    ) "user_contributed functions")


(defvar ncl-key-wrfarw
  '(
    "wrf_contour" "wrf_map" "wrf_map_overlay" "wrf_map_overlays" "wrf_map_zoom" "wrf_overlay" "wrf_overlays" "wrf_user_getvar" "wrf_user_ij_to_ll" "wrf_user_intrp2d" "wrf_user_intrp3d" "wrf_user_latlon_to_ij" "wrf_user_list_times" "wrf_user_ll_to_ij" "wrf_user_unstagger" "wrf_vector"
    ) "wrf_arw functions")


(defvar ncl-key-wrfcontrib
  '(
    "wrf_mapres_c" "wrf_times_c"
    ) "wrf_contributed functions")


(defvar ncl-key-windrose
  '(
    "WindRoseBasic" "WindRoseColor" "WindRoseThickLine"
    ) "wind_rose functions")


(defvar ncl-key-gsn
  '(
    "gsn_add_annotation" "gsn_add_polygon" "gsn_add_polyline" "gsn_add_polymarker" "gsn_add_text" "gsn_attach_plots" "gsn_blank_plot" "gsn_contour" "gsn_contour_map" "gsn_contour_shade" "gsn_create_labelbar" "gsn_create_legend" "gsn_create_text" "gsn_csm_attach_zonal_means" "gsn_csm_blank_plot" "gsn_csm_contour" "gsn_csm_contour_map" "gsn_csm_contour_map_ce" "gsn_csm_contour_map_overlay" "gsn_csm_contour_map_polar" "gsn_csm_hov" "gsn_csm_lat_time" "gsn_csm_map" "gsn_csm_map_ce" "gsn_csm_map_polar" "gsn_csm_pres_hgt" "gsn_csm_pres_hgt_streamline" "gsn_csm_pres_hgt_vector" "gsn_csm_streamline" "gsn_csm_streamline_contour_map" "gsn_csm_streamline_contour_map_ce" "gsn_csm_streamline_contour_map_polar" "gsn_csm_streamline_map" "gsn_csm_streamline_map_ce" "gsn_csm_streamline_map_polar" "gsn_csm_time_lat" "gsn_csm_vector" "gsn_csm_vector_map" "gsn_csm_vector_map_ce" "gsn_csm_vector_map_polar" "gsn_csm_vector_scalar" "gsn_csm_vector_scalar_map" "gsn_csm_vector_scalar_map_ce" "gsn_csm_vector_scalar_map_polar" "gsn_csm_x2y" "gsn_csm_x2y2" "gsn_csm_xy" "gsn_csm_xy2" "gsn_csm_xy3" "gsn_csm_y" "gsn_define_colormap" "gsn_draw_colormap" "gsn_draw_named_colors" "gsn_histogram" "gsn_labelbar_ndc" "gsn_legend_ndc" "gsn_map" "gsn_merge_colormaps" "gsn_open_wks" "gsn_panel" "gsn_polygon" "gsn_polygon_ndc" "gsn_polyline" "gsn_polyline_ndc" "gsn_polymarker" "gsn_polymarker_ndc" "gsn_retrieve_colormap" "gsn_reverse_colormap" "gsn_streamline" "gsn_streamline_map" "gsn_streamline_scalar" "gsn_streamline_scalar_map" "gsn_table" "gsn_text" "gsn_text_ndc" "gsn_vector" "gsn_vector_map" "gsn_vector_scalar" "gsn_vector_scalar_map" "gsn_xy" "gsn_y" "hsv2rgb" "maximize_output" "namedcolor2rgb" "reset_device_coordinates" "span_named_colors"
    ) "gsn csm plot templates and special gsn functions")


(defvar ncl-resources
  '(
    "amDataXF" "amDataYF" "amJust" "amOn" "amOrthogonalPosF" "amParallelPosF" "amResizeNotify" "amSide" "amTrackData" "amViewId" "amZone" "appDefaultParent" "appFileSuffix" "appResources" "appSysDir" "appUsrDir" "caCopyArrays" "caXArray" "caXCast" "caXMaxV" "caXMinV" "caXMissingV" "caYArray" "caYCast" "caYMaxV" "caYMinV" "caYMissingV" "cnCellFillEdgeColor" "cnCellFillMissingValEdgeColor" "cnConpackParams" "cnConstFLabelAngleF" "cnConstFLabelBackgroundColor" "cnConstFLabelConstantSpacingF" "cnConstFLabelFont" "cnConstFLabelFontAspectF" "cnConstFLabelFontColor" "cnConstFLabelFontHeightF" "cnConstFLabelFontQuality" "cnConstFLabelFontThicknessF" "cnConstFLabelFormat" "cnConstFLabelFuncCode" "cnConstFLabelJust" "cnConstFLabelOn" "cnConstFLabelOrthogonalPosF" "cnConstFLabelParallelPosF" "cnConstFLabelPerimColor" "cnConstFLabelPerimOn" "cnConstFLabelPerimSpaceF" "cnConstFLabelPerimThicknessF" "cnConstFLabelSide" "cnConstFLabelString" "cnConstFLabelTextDirection" "cnConstFLabelZone" "cnConstFUseInfoLabelRes" "cnExplicitLabelBarLabelsOn" "cnExplicitLegendLabelsOn" "cnExplicitLineLabelsOn" "cnFillBackgroundColor" "cnFillColor" "cnFillColors" "cnFillDotSizeF" "cnFillDrawOrder" "cnFillMode" "cnFillOn" "cnFillPattern" "cnFillPatterns" "cnFillScaleF" "cnFillScales" "cnFixFillBleed" "cnGridBoundPerimColor" "cnGridBoundPerimDashPattern" "cnGridBoundPerimOn" "cnGridBoundPerimThicknessF" "cnHighLabelAngleF" "cnHighLabelBackgroundColor" "cnHighLabelConstantSpacingF" "cnHighLabelCount" "cnHighLabelFont" "cnHighLabelFontAspectF" "cnHighLabelFontColor" "cnHighLabelFontHeightF" "cnHighLabelFontQuality" "cnHighLabelFontThicknessF" "cnHighLabelFormat" "cnHighLabelFuncCode" "cnHighLabelPerimColor" "cnHighLabelPerimOn" "cnHighLabelPerimSpaceF" "cnHighLabelPerimThicknessF" "cnHighLabelString" "cnHighLabelsOn" "cnHighLowLabelOverlapMode" "cnHighUseLineLabelRes" "cnInfoLabelAngleF" "cnInfoLabelBackgroundColor" "cnInfoLabelConstantSpacingF" "cnInfoLabelFont" "cnInfoLabelFontAspectF" "cnInfoLabelFontColor" "cnInfoLabelFontHeightF" "cnInfoLabelFontQuality" "cnInfoLabelFontThicknessF" "cnInfoLabelFormat" "cnInfoLabelFuncCode" "cnInfoLabelJust" "cnInfoLabelOn" "cnInfoLabelOrthogonalPosF" "cnInfoLabelParallelPosF" "cnInfoLabelPerimColor" "cnInfoLabelPerimOn" "cnInfoLabelPerimSpaceF" "cnInfoLabelPerimThicknessF" "cnInfoLabelSide" "cnInfoLabelString" "cnInfoLabelTextDirection" "cnInfoLabelZone" "cnLabelBarEndLabelsOn" "cnLabelBarEndStyle" "cnLabelDrawOrder" "cnLabelMasking" "cnLabelScaleFactorF" "cnLabelScaleValueF" "cnLabelScalingMode" "cnLegendLevelFlags" "cnLevelCount" "cnLevelFlag" "cnLevelFlags" "cnLevelSelectionMode" "cnLevelSpacingF" "cnLevels" "cnLineColor" "cnLineColors" "cnLineDashPattern" "cnLineDashPatterns" "cnLineDashSegLenF" "cnLineDrawOrder" "cnLineLabelAngleF" "cnLineLabelBackgroundColor" "cnLineLabelConstantSpacingF" "cnLineLabelCount" "cnLineLabelDensityF" "cnLineLabelFont" "cnLineLabelFontAspectF" "cnLineLabelFontColor" "cnLineLabelFontColors" "cnLineLabelFontHeightF" "cnLineLabelFontQuality" "cnLineLabelFontThicknessF" "cnLineLabelFormat" "cnLineLabelFuncCode" "cnLineLabelInterval" "cnLineLabelPerimColor" "cnLineLabelPerimOn" "cnLineLabelPerimSpaceF" "cnLineLabelPerimThicknessF" "cnLineLabelPlacementMode" "cnLineLabelStrings" "cnLineLabelsOn" "cnLineThicknessF" "cnLineThicknesses" "cnLinesOn" "cnLowLabelAngleF" "cnLowLabelBackgroundColor" "cnLowLabelConstantSpacingF" "cnLowLabelCount" "cnLowLabelFont" "cnLowLabelFontAspectF" "cnLowLabelFontColor" "cnLowLabelFontHeightF" "cnLowLabelFontQuality" "cnLowLabelFontThicknessF" "cnLowLabelFormat" "cnLowLabelFuncCode" "cnLowLabelPerimColor" "cnLowLabelPerimOn" "cnLowLabelPerimSpaceF" "cnLowLabelPerimThicknessF" "cnLowLabelString" "cnLowLabelsOn" "cnLowUseHighLabelRes" "cnMaxDataValueFormat" "cnMaxLevelCount" "cnMaxLevelValF" "cnMaxPointDistanceF" "cnMinLevelValF" "cnMissingValFillColor" "cnMissingValFillPattern" "cnMissingValFillScaleF" "cnMissingValPerimColor" "cnMissingValPerimDashPattern" "cnMissingValPerimGridBoundOn" "cnMissingValPerimOn" "cnMissingValPerimThicknessF" "cnMonoFillColor" "cnMonoFillPattern" "cnMonoFillScale" "cnMonoLevelFlag" "cnMonoLineColor" "cnMonoLineDashPattern" "cnMonoLineLabelFontColor"
    "cnMonoLineThickness" "cnNoDataLabelOn" "cnNoDataLabelString" "cnOutOfRangePerimColor" "cnOutOfRangePerimDashPattern" "cnOutOfRangePerimOn" "cnOutOfRangePerimThicknessF" "cnRasterCellSizeF" "cnRasterMinCellSizeF" "cnRasterModeOn" "cnRasterSampleFactorF" "cnRasterSmoothingOn" "cnScalarFieldData" "cnSmoothingDistanceF" "cnSmoothingOn" "cnSmoothingTensionF" "ctCopyTables" "ctXElementSize" "ctXMaxV" "ctXMinV" "ctXMissingV" "ctXTable" "ctXTableLengths" "ctXTableType" "ctYElementSize" "ctYMaxV" "ctYMinV" "ctYMissingV" "ctYTable" "ctYTableLengths" "ctYTableType" "dcDelayCompute" "errBuffer" "errFileName" "errFilePtr" "errLevel" "errPrint" "errUnitNumber" "gsClipOn" "gsEdgeColor" "gsEdgeDashPattern" "gsEdgeDashSegLenF" "gsEdgeThicknessF" "gsEdgesOn" "gsFillBackgroundColor" "gsFillColor" "gsFillDotSizeF" "gsFillIndex" "gsFillLineThicknessF" "gsFillScaleF" "gsFont" "gsFontAspectF" "gsFontColor" "gsFontHeightF" "gsFontQuality" "gsFontThicknessF" "gsLineColor" "gsLineDashPattern" "gsLineDashSegLenF" "gsLineLabelConstantSpacingF" "gsLineLabelFont" "gsLineLabelFontAspectF" "gsLineLabelFontColor" "gsLineLabelFontHeightF" "gsLineLabelFontQuality" "gsLineLabelFontThicknessF" "gsLineLabelFuncCode" "gsLineLabelString" "gsLineThicknessF" "gsMarkerColor" "gsMarkerIndex" "gsMarkerSizeF" "gsMarkerThicknessF" "gsTextAngleF" "gsTextConstantSpacingF" "gsTextDirection" "gsTextFuncCode" "gsTextJustification" "gsnAboveYRefLineBarColors" "gsnAboveYRefLineBarFillScales" "gsnAboveYRefLineBarPatterns" "gsnAboveYRefLineColor" "gsnAddCyclic" "gsnAttachBorderOn" "gsnAttachPlotsXAxis" "gsnBelowYRefLineBarColors" "gsnBelowYRefLineBarFillScales" "gsnBelowYRefLineBarPatterns" "gsnBelowYRefLineColor" "gsnBoxMargin" "gsnCenterString" "gsnCenterStringFontColor" "gsnCenterStringFontHeightF" "gsnCenterStringOrthogonalPosF" "gsnCenterStringParallelPosF" "gsnContourLineThicknessesScale" "gsnContourNegLineDashPattern" "gsnContourPosLineDashPattern" "gsnContourZeroLineThicknessF" "gsnDebugWriteFileName" "gsnDraw" "gsnFrame" "gsnHistogramBarWidthPercent" "gsnHistogramBinIntervals" "gsnHistogramBinMissing" "gsnHistogramBinWidth" "gsnHistogramClassIntervals" "gsnHistogramCompare" "gsnHistogramComputePercentages" "gsnHistogramComputePercentagesNoMissing" "gsnHistogramDiscreteBinValues" "gsnHistogramDiscreteClassValues" "gsnHistogramHorizontal" "gsnHistogramMinMaxBinsOn" "gsnHistogramNumberOfBins" "gsnHistogramPercentSign" "gsnHistogramSelectNiceIntervals" "gsnLeftString" "gsnLeftStringFontColor" "gsnLeftStringFontHeightF" "gsnLeftStringOrthogonalPosF" "gsnLeftStringParallelPosF" "gsnMajorLatSpacing" "gsnMajorLonSpacing" "gsnMaskLambertConformal" "gsnMaskLambertConformalOutlineOn" "gsnMaximize" "gsnMinorLatSpacing" "gsnMinorLonSpacing" "gsnPanelBottom" "gsnPanelCenter" "gsnPanelDebug" "gsnPanelFigureStrings" "gsnPanelFigureStringsBackgroundFillColor" "gsnPanelFigureStringsFontHeightF" "gsnPanelFigureStringsPerimOn" "gsnPanelLabelBar" "gsnPanelLeft" "gsnPanelRight" "gsnPanelRowSpec" "gsnPanelScalePlotIndex" "gsnPanelTop" "gsnPanelXF" "gsnPanelXWhiteSpacePercent" "gsnPanelYF" "gsnPanelYWhiteSpacePercent" "gsnPaperHeight" "gsnPaperMargin" "gsnPaperOrientation" "gsnPaperWidth" "gsnPolar" "gsnPolarLabelDistance" "gsnPolarLabelFont" "gsnPolarLabelFontHeightF" "gsnPolarLabelSpacing" "gsnPolarTime" "gsnPolarUT" "gsnRightString" "gsnRightStringFontColor" "gsnRightStringFontHeightF" "gsnRightStringOrthogonalPosF" "gsnRightStringParallelPosF" "gsnScalarContour" "gsnScale" "gsnShape" "gsnSpreadColorEnd" "gsnSpreadColorStart" "gsnSpreadColors" "gsnStringFont" "gsnStringFontColor" "gsnStringFontHeightF" "gsnTickMarksOn" "gsnXAxisIrregular2Linear" "gsnXAxisIrregular2Log" "gsnXRefLine" "gsnXRefLineColor" "gsnXRefLineDashPattern" "gsnXRefLineThicknessF" "gsnXYAboveFillColors" "gsnXYBarChart" "gsnXYBarChartBarWidth" "gsnXYBarChartColors" "gsnXYBarChartColors2" "gsnXYBarChartOutlineOnly" "gsnXYBarChartPatterns" "gsnXYBarChartPatterns2" "gsnXYBelowFillColors" "gsnXYFillColors" "gsnYAxisIrregular2Linear" "gsnYAxisIrregular2Log" "gsnYRefLine" "gsnYRefLineColor" "gsnYRefLineColors" "gsnYRefLineDashPattern" "gsnYRefLineDashPatterns" "gsnYRefLineThicknessF" "gsnYRefLineThicknesses" "gsnZonalMean" "gsnZonalMeanXMaxF" "gsnZonalMeanXMinF"
    "gsnZonalMeanYRefLine" "lbAutoManage" "lbBottomMarginF" "lbBoxCount" "lbBoxFractions" "lbBoxLineColor" "lbBoxLineDashPattern" "lbBoxLineDashSegLenF" "lbBoxLineThicknessF" "lbBoxLinesOn" "lbBoxMajorExtentF" "lbBoxMinorExtentF" "lbBoxSizing" "lbFillBackground" "lbFillColor" "lbFillColors" "lbFillDotSizeF" "lbFillLineThicknessF" "lbFillPattern" "lbFillPatterns" "lbFillScaleF" "lbFillScales" "lbJustification" "lbLabelAlignment" "lbLabelAngleF" "lbLabelAutoStride" "lbLabelBarOn" "lbLabelConstantSpacingF" "lbLabelDirection" "lbLabelFont" "lbLabelFontAspectF" "lbLabelFontColor" "lbLabelFontHeightF" "lbLabelFontQuality" "lbLabelFontThicknessF" "lbLabelFuncCode" "lbLabelJust" "lbLabelOffsetF" "lbLabelPosition" "lbLabelStride" "lbLabelStrings" "lbLabelsOn" "lbLeftMarginF" "lbMaxLabelLenF" "lbMinLabelSpacingF" "lbMonoFillColor" "lbMonoFillPattern" "lbMonoFillScale" "lbOrientation" "lbPerimColor" "lbPerimDashPattern" "lbPerimDashSegLenF" "lbPerimFill" "lbPerimFillColor" "lbPerimOn" "lbPerimThicknessF" "lbRasterFillOn" "lbRightMarginF" "lbTitleAngleF" "lbTitleConstantSpacingF" "lbTitleDirection" "lbTitleExtentF" "lbTitleFont" "lbTitleFontAspectF" "lbTitleFontColor" "lbTitleFontHeightF" "lbTitleFontQuality" "lbTitleFontThicknessF" "lbTitleFuncCode" "lbTitleJust" "lbTitleOffsetF" "lbTitleOn" "lbTitlePosition" "lbTitleString" "lbTopMarginF" "lgAutoManage" "lgBottomMarginF" "lgBoxBackground" "lgBoxLineColor" "lgBoxLineDashPattern" "lgBoxLineDashSegLenF" "lgBoxLineThicknessF" "lgBoxLinesOn" "lgBoxMajorExtentF" "lgBoxMinorExtentF" "lgDashIndex" "lgDashIndexes" "lgItemCount" "lgItemOrder" "lgItemPlacement" "lgItemPositions" "lgItemType" "lgItemTypes" "lgJustification" "lgLabelAlignment" "lgLabelAngleF" "lgLabelAutoStride" "lgLabelConstantSpacingF" "lgLabelDirection" "lgLabelFont" "lgLabelFontAspectF" "lgLabelFontColor" "lgLabelFontHeightF" "lgLabelFontQuality" "lgLabelFontThicknessF" "lgLabelFuncCode" "lgLabelJust" "lgLabelOffsetF" "lgLabelPosition" "lgLabelStride" "lgLabelStrings" "lgLabelsOn" "lgLeftMarginF" "lgLegendOn" "lgLineColor" "lgLineColors" "lgLineDashSegLenF" "lgLineDashSegLens" "lgLineLabelConstantSpacingF" "lgLineLabelFont" "lgLineLabelFontAspectF" "lgLineLabelFontColor" "lgLineLabelFontColors" "lgLineLabelFontHeightF" "lgLineLabelFontHeights" "lgLineLabelFontQuality" "lgLineLabelFontThicknessF" "lgLineLabelFuncCode" "lgLineLabelStrings" "lgLineLabelsOn" "lgLineThicknessF" "lgLineThicknesses" "lgMarkerColor" "lgMarkerColors" "lgMarkerIndex" "lgMarkerIndexes" "lgMarkerSizeF" "lgMarkerSizes" "lgMarkerThicknessF" "lgMarkerThicknesses" "lgMonoDashIndex" "lgMonoItemType" "lgMonoLineColor" "lgMonoLineDashSegLen" "lgMonoLineLabelFontColor" "lgMonoLineLabelFontHeight" "lgMonoLineThickness" "lgMonoMarkerColor" "lgMonoMarkerIndex" "lgMonoMarkerSize" "lgMonoMarkerThickness" "lgOrientation" "lgPerimColor" "lgPerimDashPattern" "lgPerimDashSegLenF" "lgPerimFill" "lgPerimFillColor" "lgPerimOn" "lgPerimThicknessF" "lgRightMarginF" "lgTitleAngleF" "lgTitleConstantSpacingF" "lgTitleDirection" "lgTitleExtentF" "lgTitleFont" "lgTitleFontAspectF" "lgTitleFontColor" "lgTitleFontHeightF" "lgTitleFontQuality" "lgTitleFontThicknessF" "lgTitleFuncCode" "lgTitleJust" "lgTitleOffsetF" "lgTitleOn" "lgTitlePosition" "lgTitleString" "lgTopMarginF" "mpAreaGroupCount" "mpAreaMaskingOn" "mpAreaNames" "mpAreaTypes" "mpBottomAngleF" "mpBottomMapPosF" "mpBottomNDCF" "mpBottomNPCF" "mpBottomPointLatF" "mpBottomPointLonF" "mpBottomWindowF" "mpCenterLatF" "mpCenterLonF" "mpCenterRotF" "mpDataBaseVersion" "mpDataResolution" "mpDataSetName" "mpDefaultFillColor" "mpDefaultFillPattern" "mpDefaultFillScaleF" "mpDynamicAreaGroups" "mpEllipticalBoundary" "mpFillAreaSpecifiers"
    "mpFillBoundarySets" "mpFillColor" "mpFillColors" "mpFillDotSizeF" "mpFillDrawOrder" "mpFillOn" "mpFillPatternBackground" "mpFillPattern" "mpFillPatterns" "mpFillScaleF" "mpFillScales" "mpFixedAreaGroups" "mpGeophysicalLineColor" "mpGeophysicalLineDashPattern" "mpGeophysicalLineDashSegLenF" "mpGeophysicalLineThicknessF" "mpGreatCircleLinesOn" "mpGridAndLimbDrawOrder" "mpGridAndLimbOn" "mpGridLatSpacingF" "mpGridLineColor" "mpGridLineDashPattern" "mpGridLineDashSegLenF" "mpGridLineThicknessF" "mpGridLonSpacingF" "mpGridMaskMode" "mpGridMaxLatF" "mpGridPolarLonSpacingF" "mpGridSpacingF" "mpInlandWaterFillColor" "mpInlandWaterFillPattern" "mpInlandWaterFillScaleF" "mpLabelDrawOrder" "mpLabelFontColor" "mpLabelFontHeightF" "mpLabelsOn" "mpLambertMeridianF" "mpLambertParallel1F" "mpLambertParallel2F" "mpLandFillColor" "mpLandFillPattern" "mpLandFillScaleF" "mpLeftAngleF" "mpLeftCornerLatF" "mpLeftCornerLonF" "mpLeftMapPosF" "mpLeftNDCF" "mpLeftNPCF" "mpLeftPointLatF" "mpLeftPointLonF" "mpLeftWindowF" "mpLimbLineColor" "mpLimbLineDashPattern" "mpLimbLineDashSegLenF" "mpLimbLineThicknessF" "mpLimitMode" "mpMaskAreaSpecifiers" "mpMaskOutlineSpecifiers" "mpMaxLatF" "mpMaxLonF" "mpMinLatF" "mpMinLonF" "mpMonoFillColor" "mpMonoFillPattern" "mpMonoFillScale" "mpNationalLineColor" "mpNationalLineDashPattern" "mpNationalLineDashSegLenF" "mpNationalLineThicknessF" "mpOceanFillColor" "mpOceanFillPattern" "mpOceanFillScaleF" "mpOutlineBoundarySets" "mpOutlineDrawOrder" "mpOutlineMaskingOn" "mpOutlineOn" "mpOutlineSpecifiers" "mpPerimDrawOrder" "mpPerimLineColor" "mpPerimLineDashPattern" "mpPerimLineDashSegLenF" "mpPerimLineThicknessF" "mpPerimOn" "mpProjection" "mpRelativeCenterLat" "mpRelativeCenterLon" "mpRightAngleF" "mpRightCornerLatF" "mpRightCornerLonF" "mpRightMapPosF" "mpRightNDCF" "mpRightNPCF" "mpRightPointLatF" "mpRightPointLonF" "mpRightWindowF" "mpSatelliteAngle1F" "mpSatelliteAngle2F" "mpSatelliteDistF" "mpShapeMode" "mpSpecifiedFillColors" "mpSpecifiedFillDirectIndexing" "mpSpecifiedFillPatterns" "mpSpecifiedFillPriority" "mpSpecifiedFillScales" "mpTopAngleF" "mpTopMapPosF" "mpTopNDCF" "mpTopNPCF" "mpTopPointLatF" "mpTopPointLonF" "mpTopWindowF" "mpUSStateLineColor" "mpUSStateLineDashPattern" "mpUSStateLineDashSegLenF" "mpUSStateLineThicknessF" "pmAnnoManagers" "pmAnnoViews" "pmLabelBarDisplayMode" "pmLabelBarHeightF" "pmLabelBarKeepAspect" "pmLabelBarOrthogonalPosF" "pmLabelBarParallelPosF" "pmLabelBarSide" "pmLabelBarWidthF" "pmLabelBarZone" "pmLegendDisplayMode" "pmLegendHeightF" "pmLegendKeepAspect" "pmLegendOrthogonalPosF" "pmLegendParallelPosF" "pmLegendSide" "pmLegendWidthF" "pmLegendZone" "pmOverlaySequenceIds" "pmTickMarkDisplayMode" "pmTickMarkZone" "pmTitleDisplayMode" "pmTitleZone" "prGraphicStyle" "prPolyType" "prXArray" "prYArray" "sfCopyData" "sfCopyData" "sfDataArray" "sfDataArray" "sfDataMaxV" "sfDataMaxV" "sfDataMinV" "sfDataMinV" "sfElementNodes" "sfExchangeDimensions" "sfFirstNodeIndex" "sfMissingValueV" "sfMissingValueV" "sfXArray" "sfXArray" "sfXCActualEndF" "sfXCActualEndF" "sfXCActualStartF" "sfXCActualStartF" "sfXCEndIndex" "sfXCEndSubsetV" "sfXCEndV" "sfXCStartIndex" "sfXCStartSubsetV" "sfXCStartV" "sfXCStride" "sfXCellBounds" "sfYArray" "sfYArray" "sfYCActualEndF" "sfYCActualEndF" "sfYCActualStartF" "sfYCActualStartF" "sfYCEndIndex" "sfYCEndSubsetV" "sfYCEndV" "sfYCStartIndex" "sfYCStartSubsetV" "sfYCStartV" "sfYCStride" "sfYCellBounds" "stArrowLengthF" "stArrowStride" "stCrossoverCheckCount" "stExplicitLabelBarLabelsOn" "stLabelBarEndLabelsOn" "stLabelFormat" "stLengthCheckCount" "stLevelColors" "stLevelCount" "stLevelSelectionMode" "stLevelSpacingF" "stLevels" "stLineColor" "stLineStartStride" "stLineThicknessF" "stMapDirection" "stMaxLevelCount"
    "stMaxLevelValF" "stMinArrowSpacingF" "stMinDistanceF" "stMinLevelValF" "stMinLineSpacingF" "stMinStepFactorF" "stMonoLineColor" "stNoDataLabelOn" "stNoDataLabelString" "stScalarFieldData" "stScalarMissingValColor" "stStepSizeF" "stStreamlineDrawOrder" "stUseScalarArray" "stVectorFieldData" "stZeroFLabelAngleF" "stZeroFLabelBackgroundColor" "stZeroFLabelConstantSpacingF" "stZeroFLabelFont" "stZeroFLabelFontAspectF" "stZeroFLabelFontColor" "stZeroFLabelFontHeightF" "stZeroFLabelFontQuality" "stZeroFLabelFontThicknessF" "stZeroFLabelFuncCode" "stZeroFLabelJust" "stZeroFLabelOn" "stZeroFLabelOrthogonalPosF" "stZeroFLabelParallelPosF" "stZeroFLabelPerimColor" "stZeroFLabelPerimOn" "stZeroFLabelPerimSpaceF" "stZeroFLabelPerimThicknessF" "stZeroFLabelSide" "stZeroFLabelString" "stZeroFLabelTextDirection" "stZeroFLabelZone" "tfDoNDCOverlay" "tfPlotManagerOn" "tfPolyDrawList" "tfPolyDrawOrder" "tiDeltaF" "tiMainAngleF" "tiMainConstantSpacingF" "tiMainDirection" "tiMainFont" "tiMainFontAspectF" "tiMainFontColor" "tiMainFontHeightF" "tiMainFontQuality" "tiMainFontThicknessF" "tiMainFuncCode" "tiMainJust" "tiMainOffsetXF" "tiMainOffsetYF" "tiMainOn" "tiMainPosition" "tiMainSide" "tiMainString" "tiUseMainAttributes" "tiXAxisAngleF" "tiXAxisConstantSpacingF" "tiXAxisDirection" "tiXAxisFont" "tiXAxisFontAspectF" "tiXAxisFontColor" "tiXAxisFontHeightF" "tiXAxisFontQuality" "tiXAxisFontThicknessF" "tiXAxisFuncCode" "tiXAxisJust" "tiXAxisOffsetXF" "tiXAxisOffsetYF" "tiXAxisOn" "tiXAxisPosition" "tiXAxisSide" "tiXAxisString" "tiYAxisAngleF" "tiYAxisConstantSpacingF" "tiYAxisDirection" "tiYAxisFont" "tiYAxisFontAspectF" "tiYAxisFontColor" "tiYAxisFontHeightF" "tiYAxisFontQuality" "tiYAxisFontThicknessF" "tiYAxisFuncCode" "tiYAxisJust" "tiYAxisOffsetXF" "tiYAxisOffsetYF" "tiYAxisOn" "tiYAxisPosition" "tiYAxisSide" "tiYAxisString" "tmBorderLineColor" "tmBorderThicknessF" "tmEqualizeXYSizes" "tmLabelAutoStride" "tmSciNoteCutoff" "tmXBAutoPrecision" "tmXBBorderOn" "tmXBDataLeftF" "tmXBDataRightF" "tmXBFormat" "tmXBIrrTensionF" "tmXBIrregularPoints" "tmXBLabelAngleF" "tmXBLabelConstantSpacingF" "tmXBLabelDeltaF" "tmXBLabelDirection" "tmXBLabelFont" "tmXBLabelFontAspectF" "tmXBLabelFontColor" "tmXBLabelFontHeightF" "tmXBLabelFontQuality" "tmXBLabelFontThicknessF" "tmXBLabelFuncCode" "tmXBLabelJust" "tmXBLabelStride" "tmXBLabels" "tmXBLabelsOn" "tmXBMajorLengthF" "tmXBMajorLineColor" "tmXBMajorOutwardLengthF" "tmXBMajorThicknessF" "tmXBMaxLabelLenF" "tmXBMaxTicks" "tmXBMinLabelSpacingF" "tmXBMinorLengthF" "tmXBMinorLineColor" "tmXBMinorOn" "tmXBMinorOutwardLengthF" "tmXBMinorPerMajor" "tmXBMinorThicknessF" "tmXBMinorValues" "tmXBMode" "tmXBOn" "tmXBPrecision" "tmXBStyle" "tmXBTickEndF" "tmXBTickSpacingF" "tmXBTickStartF" "tmXBValues" "tmXMajorGrid" "tmXMajorGridLineColor" "tmXMajorGridLineDashPattern" "tmXMajorGridThicknessF" "tmXMinorGrid" "tmXMinorGridLineColor" "tmXMinorGridLineDashPattern" "tmXMinorGridThicknessF" "tmXTAutoPrecision" "tmXTBorderOn" "tmXTDataLeftF" "tmXTDataRightF" "tmXTFormat" "tmXTIrrTensionF" "tmXTIrregularPoints" "tmXTLabelAngleF" "tmXTLabelConstantSpacingF" "tmXTLabelDeltaF" "tmXTLabelDirection" "tmXTLabelFont" "tmXTLabelFontAspectF" "tmXTLabelFontColor" "tmXTLabelFontHeightF" "tmXTLabelFontQuality" "tmXTLabelFontThicknessF" "tmXTLabelFuncCode" "tmXTLabelJust" "tmXTLabelStride" "tmXTLabels" "tmXTLabelsOn" "tmXTMajorLengthF" "tmXTMajorLineColor" "tmXTMajorOutwardLengthF" "tmXTMajorThicknessF" "tmXTMaxLabelLenF" "tmXTMaxTicks" "tmXTMinLabelSpacingF" "tmXTMinorLengthF" "tmXTMinorLineColor" "tmXTMinorOn" "tmXTMinorOutwardLengthF" "tmXTMinorPerMajor" "tmXTMinorThicknessF" "tmXTMinorValues" "tmXTMode" "tmXTOn" "tmXTPrecision" "tmXTStyle" "tmXTTickEndF" "tmXTTickSpacingF" "tmXTTickStartF" "tmXTValues" "tmXUseBottom" "tmYLAutoPrecision" "tmYLBorderOn" "tmYLDataBottomF" "tmYLDataTopF"
    "tmYLFormat" "tmYLIrrTensionF" "tmYLIrregularPoints" "tmYLLabelAngleF" "tmYLLabelConstantSpacingF" "tmYLLabelDeltaF" "tmYLLabelDirection" "tmYLLabelFont" "tmYLLabelFontAspectF" "tmYLLabelFontColor" "tmYLLabelFontHeightF" "tmYLLabelFontQuality" "tmYLLabelFontThicknessF" "tmYLLabelFuncCode" "tmYLLabelJust" "tmYLLabelStride" "tmYLLabels" "tmYLLabelsOn" "tmYLMajorLengthF" "tmYLMajorLineColor" "tmYLMajorOutwardLengthF" "tmYLMajorThicknessF" "tmYLMaxLabelLenF" "tmYLMaxTicks" "tmYLMinLabelSpacingF" "tmYLMinorLengthF" "tmYLMinorLineColor" "tmYLMinorOn" "tmYLMinorOutwardLengthF" "tmYLMinorPerMajor" "tmYLMinorThicknessF" "tmYLMinorValues" "tmYLMode" "tmYLOn" "tmYLPrecision" "tmYLStyle" "tmYLTickEndF" "tmYLTickSpacingF" "tmYLTickStartF" "tmYLValues" "tmYMajorGrid" "tmYMajorGridLineColor" "tmYMajorGridLineDashPattern" "tmYMajorGridThicknessF" "tmYMinorGrid" "tmYMinorGridLineColor" "tmYMinorGridLineDashPattern" "tmYMinorGridThicknessF" "tmYRAutoPrecision" "tmYRBorderOn" "tmYRDataBottomF" "tmYRDataTopF" "tmYRFormat" "tmYRIrrTensionF" "tmYRIrregularPoints" "tmYRLabelAngleF" "tmYRLabelConstantSpacingF" "tmYRLabelDeltaF" "tmYRLabelDirection" "tmYRLabelFont" "tmYRLabelFontAspectF" "tmYRLabelFontColor" "tmYRLabelFontHeightF" "tmYRLabelFontQuality" "tmYRLabelFontThicknessF" "tmYRLabelFuncCode" "tmYRLabelJust" "tmYRLabelStride" "tmYRLabels" "tmYRLabelsOn" "tmYRMajorLengthF" "tmYRMajorLineColor" "tmYRMajorOutwardLengthF" "tmYRMajorThicknessF" "tmYRMaxLabelLenF" "tmYRMaxTicks" "tmYRMinLabelSpacingF" "tmYRMinorLengthF" "tmYRMinorLineColor" "tmYRMinorOn" "tmYRMinorOutwardLengthF" "tmYRMinorPerMajor" "tmYRMinorThicknessF" "tmYRMinorValues" "tmYRMode" "tmYROn" "tmYRPrecision" "tmYRStyle" "tmYRTickEndF" "tmYRTickSpacingF" "tmYRTickStartF" "tmYRValues" "tmYUseLeft" "trGridType" "trLineInterpolationOn" "trXAxisType" "trXCoordPoints" "trXInterPoints" "trXLog" "trXMaxF" "trXMinF" "trXReverse" "trXSamples" "trXTensionF" "trYAxisType" "trYCoordPoints" "trYInterPoints" "trYLog" "trYMaxF" "trYMinF" "trYReverse" "trYSamples" "trYTensionF" "txAngleF" "txBackgroundFillColor" "txConstantSpacingF" "txDirection" "txFont" "txFontAspectF" "txFontColor" "txFontHeightF" "txFontQuality" "txFontThicknessF" "txFuncCode" "txJust" "txPerimColor" "txPerimDashLengthF" "txPerimDashPattern" "txPerimOn" "txPerimSpaceF" "txPerimThicknessF" "txPosXF" "txPosYF" "txString" "vcExplicitLabelBarLabelsOn" "vcFillArrowEdgeColor" "vcFillArrowEdgeThicknessF" "vcFillArrowFillColor" "vcFillArrowHeadInteriorXF" "vcFillArrowHeadMinFracXF" "vcFillArrowHeadMinFracYF" "vcFillArrowHeadXF" "vcFillArrowHeadYF" "vcFillArrowMinFracWidthF" "vcFillArrowWidthF" "vcFillArrowsOn" "vcFillOverEdge" "vcGlyphStyle" "vcLabelBarEndLabelsOn" "vcLabelFontColor" "vcLabelFontHeightF" "vcLabelsOn" "vcLabelsUseVectorColor" "vcLevelColors" "vcLevelCount" "vcLevelSelectionMode" "vcLevelSpacingF" "vcLevels" "vcLineArrowColor" "vcLineArrowHeadMaxSizeF" "vcLineArrowHeadMinSizeF" "vcLineArrowThicknessF" "vcMagnitudeFormat" "vcMagnitudeScaleFactorF" "vcMagnitudeScaleValueF" "vcMagnitudeScalingMode" "vcMapDirection" "vcMaxLevelCount" "vcMaxLevelValF" "vcMaxMagnitudeF" "vcMinAnnoAngleF" "vcMinAnnoArrowAngleF" "vcMinAnnoArrowEdgeColor" "vcMinAnnoArrowFillColor" "vcMinAnnoArrowLineColor" "vcMinAnnoArrowMinOffsetF" "vcMinAnnoArrowSpaceF" "vcMinAnnoArrowUseVecColor" "vcMinAnnoBackgroundColor" "vcMinAnnoConstantSpacingF" "vcMinAnnoExplicitMagnitudeF" "vcMinAnnoFont" "vcMinAnnoFontAspectF" "vcMinAnnoFontColor" "vcMinAnnoFontHeightF" "vcMinAnnoFontQuality" "vcMinAnnoFontThicknessF" "vcMinAnnoFuncCode" "vcMinAnnoJust" "vcMinAnnoOn" "vcMinAnnoOrientation" "vcMinAnnoOrthogonalPosF" "vcMinAnnoParallelPosF" "vcMinAnnoPerimColor" "vcMinAnnoPerimOn" "vcMinAnnoPerimSpaceF" "vcMinAnnoPerimThicknessF" "vcMinAnnoSide" "vcMinAnnoString1" "vcMinAnnoString1On"
    "vcMinAnnoString2" "vcMinAnnoString2On" "vcMinAnnoTextDirection" "vcMinAnnoZone" "vcMinDistanceF" "vcMinFracLengthF" "vcMinLevelValF" "vcMinMagnitudeF" "vcMonoFillArrowEdgeColor" "vcMonoFillArrowFillColor" "vcMonoLineArrowColor" "vcMonoWindBarbColor" "vcNoDataLabelOn" "vcNoDataLabelString" "vcPositionMode" "vcRefAnnoAngleF" "vcRefAnnoArrowAngleF" "vcRefAnnoArrowEdgeColor" "vcRefAnnoArrowFillColor" "vcRefAnnoArrowLineColor" "vcRefAnnoArrowMinOffsetF" "vcRefAnnoArrowSpaceF" "vcRefAnnoArrowUseVecColor" "vcRefAnnoBackgroundColor" "vcRefAnnoConstantSpacingF" "vcRefAnnoExplicitMagnitudeF" "vcRefAnnoFont" "vcRefAnnoFontAspectF" "vcRefAnnoFontColor" "vcRefAnnoFontHeightF" "vcRefAnnoFontQuality" "vcRefAnnoFontThicknessF" "vcRefAnnoFuncCode" "vcRefAnnoJust" "vcRefAnnoOn" "vcRefAnnoOrientation" "vcRefAnnoOrthogonalPosF" "vcRefAnnoParallelPosF" "vcRefAnnoPerimColor" "vcRefAnnoPerimOn" "vcRefAnnoPerimSpaceF" "vcRefAnnoPerimThicknessF" "vcRefAnnoSide" "vcRefAnnoString1" "vcRefAnnoString1On" "vcRefAnnoString2" "vcRefAnnoString2On" "vcRefAnnoTextDirection" "vcRefAnnoZone" "vcRefLengthF" "vcRefMagnitudeF" "vcScalarFieldData" "vcScalarMissingValColor" "vcScalarValueFormat" "vcScalarValueScaleFactorF" "vcScalarValueScaleValueF" "vcScalarValueScalingMode" "vcUseRefAnnoRes" "vcUseScalarArray" "vcVectorDrawOrder" "vcVectorFieldData" "vcWindBarbCalmCircleSizeF" "vcWindBarbColor" "vcWindBarbLineThicknessF" "vcWindBarbScaleFactorF" "vcWindBarbTickAngleF" "vcWindBarbTickLengthF" "vcWindBarbTickSpacingF" "vcZeroFLabelAngleF" "vcZeroFLabelBackgroundColor" "vcZeroFLabelConstantSpacingF" "vcZeroFLabelFont" "vcZeroFLabelFontAspectF" "vcZeroFLabelFontColor" "vcZeroFLabelFontHeightF" "vcZeroFLabelFontQuality" "vcZeroFLabelFontThicknessF" "vcZeroFLabelFuncCode" "vcZeroFLabelJust" "vcZeroFLabelOn" "vcZeroFLabelOrthogonalPosF" "vcZeroFLabelParallelPosF" "vcZeroFLabelPerimColor" "vcZeroFLabelPerimOn" "vcZeroFLabelPerimSpaceF" "vcZeroFLabelPerimThicknessF" "vcZeroFLabelSide" "vcZeroFLabelString" "vcZeroFLabelTextDirection" "vcZeroFLabelZone" "vfCopyData" "vfDataArray" "vfExchangeDimensions" "vfExchangeUVData" "vfMagMaxV" "vfMagMinV" "vfMissingUValueV" "vfMissingVValueV" "vfPolarData" "vfSingleMissingValue" "vfUDataArray" "vfUMaxV" "vfUMinV" "vfVDataArray" "vfVMaxV" "vfVMinV" "vfXArray" "vfXCActualEndF" "vfXCActualStartF" "vfXCEndIndex" "vfXCEndSubsetV" "vfXCEndV" "vfXCStartIndex" "vfXCStartSubsetV" "vfXCStartV" "vfXCStride" "vfYArray" "vfYCActualEndF" "vfYCActualStartF" "vfYCEndIndex" "vfYCEndSubsetV" "vfYCEndV" "vfYCStartIndex" "vfYCStartSubsetV" "vfYCStartV" "vfYCStride" "vpAnnoManagerId" "vpClipOn" "vpHeightF" "vpKeepAspect" "vpOn" "vpUseSegments" "vpWidthF" "vpXF" "vpYF" "wkBackgroundColor" "wkColorMapLen" "wkColorMap" "wkColorModel" "wkColorModel" "wkDashTableLength" "wkDefGraphicStyleId" "wkDeviceLowerX" "wkDeviceLowerX" "wkDeviceLowerY" "wkDeviceLowerY" "wkDeviceUpperX" "wkDeviceUpperX" "wkDeviceUpperY" "wkDeviceUpperY" "wkFillTableLength" "wkForegroundColor" "wkFullBackground" "wkFullBackground" "wkGksWorkId" "wkMarkerTableLength" "wkMetaName" "wkOrientation" "wkOrientation" "wkPDFFileName" "wkPDFFormat" "wkPDFResolution" "wkPSFileName" "wkPSFormat" "wkPSResolution" "wkPause" "wkTopLevelViews" "wkViews" "wkVisualType" "wkVisualType" "wkWindowId" "wkXColorMode" "wsCurrentSize" "wsMaximumSize" "wsThresholdSize" "xyComputeXMax" "xyComputeXMin" "xyComputeYMax" "xyComputeYMin" "xyCoordData" "xyCoordDataSpec" "xyCurveDrawOrder" "xyDashPattern" "xyDashPatterns" "xyExplicitLabels" "xyExplicitLegendLabels" "xyLabelMode" "xyLineColor" "xyLineColors" "xyLineDashSegLenF" "xyLineLabelConstantSpacingF" "xyLineLabelFont" "xyLineLabelFontAspectF" "xyLineLabelFontColor" "xyLineLabelFontColors" "xyLineLabelFontHeightF" "xyLineLabelFontQuality" "xyLineLabelFontThicknessF" "xyLineLabelFuncCode" "xyLineThicknessF"
    "xyLineThicknesses" "xyMarkLineMode" "xyMarkLineModes" "xyMarker" "xyMarkerColor" "xyMarkerColors" "xyMarkerSizeF" "xyMarkerSizes" "xyMarkerThicknessF" "xyMarkerThicknesses" "xyMarkers" "xyMonoDashPattern" "xyMonoLineColor" "xyMonoLineLabelFontColor" "xyMonoLineThickness" "xyMonoMarkLineMode" "xyMonoMarker" "xyMonoMarkerColor" "xyMonoMarkerSize" "xyMonoMarkerThickness" "xyXIrrTensionF" "xyXIrregularPoints" "xyXStyle" "xyYIrrTensionF" "xyYIrregularPoints" "xyYStyle"
    ) "Ncl resources")

;;;; COOKIE: ENDS HERE =DO NOT DELETE=

(defvar ncl-var-re
  (concat
   "^[ \t]*"                            ;initial optional space
   "\\([a-z0-9_]*\\)"                   ;var
   "[@]?.*"                             ;optional "@" and space
   "[ \t]*=.*"                          ;and rest
   )
  "Regexp for matching variable")

(defconst ncl-font-lock-keywords
  (eval-when-compile            ; for  faster loading (is it working?)
    `(;; ncl major keywords
      (,(concat
         "\\<" (regexp-opt ncl-keywords 'paren) "\\>")
       (1 font-lock-keyword-face))

      ;; operators
      (,(concat
         "\\(" (regexp-opt ncl-key-operators 'paren) "\\)")
       (1 font-lock-type-face))

      ;;"ncl built-in functions",
      (,(concat
         "\\<" (regexp-opt ncl-key-builtin 'paren) "\\>")
       (1 font-lock-builtin-face))

      ;; contrib functions
      (,(concat
         "\\<" (regexp-opt
                (nconc ncl-key-contrib ncl-key-shea ncl-key-pop
                       ncl-key-skewt ncl-key-diag ncl-key-user ncl-key-wrfarw
                       ncl-key-wrfcontrib ncl-key-windrose
                       ) 'paren) "\\>")
       (1 font-lock-function-face))

      ;; ncl gsn function-face
      (,(concat
         "\\<" (regexp-opt ncl-key-gsn 'paren) "\\>")
       (1 font-lock-variable-name-face))

      ;; ncl resources
      (,(concat
         "\\<" (regexp-opt ncl-resources t) "\\>")
       (1 font-lock-constant-face))

      ;; variable face seq`ncl-var-re'
      (,ncl-var-re (1 font-lock-variable-name-face))
      ))
  "ncl font lock key words ")

(put 'ncl-mode 'font-lock-defaults 'ncl-font-lock-keywords)

;;=================================================================
;; imenu
;;=================================================================
;;; imenu support for ncl-mode
(defcustom ncl-imenu-generic-expression
  `(("functions" "^[[:blank:]]*function[[:blank:]]+\\(.*\\)(.*)" 1)
    ("procedures" "^[[:blank:]]*procedure[[:blank:]]+\\(.*\\)(.*)" 1)
    ,(list "variables" ncl-var-re 1))
  "Generic expression for matching functions and procedure"
  :type 'string
  :group 'ncl)

(defun ncl-add-imenu-menu ()
  "Adds an \"imenu\" menu to the menubar. The look up can be customized with
`ncl-imenu-generic-expression'"
  (interactive)
  (imenu-add-to-menubar "Imenu")
  (redraw-frame (selected-frame)))

;;************************************************
;; some variables used in the creation of ncl-mode
;;************************************************
(defvar ncl-mode-map nil
  "Keymap used in NCL mode.")
(defvar ncl-startup-message t
  "*Non-nil displays a startup message when `ncl-mode' is first called.")
(defconst ncl-mode-version "v0.92")
;;************************************************
;; syntax table
;;************************************************
;; characters are preceeded by a ?.
;; "." indicates punctuation
;; "_" indicates a symbol
;; "\"" indicates a string (must escape the double-quote)
;; "<" indicates a comment
;; "w" indicates a word character

(defvar ncl-mode-syntax-table nil
  "Syntax table in use in `ncl-mode' buffers.")
(if ncl-mode-syntax-table ()
  (setq ncl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\;  "<"  ncl-mode-syntax-table)
  (modify-syntax-entry ?+   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?-   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?*   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?/   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?^   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?#   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?=   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?%   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?<   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?>   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?\'  "\"" ncl-mode-syntax-table)
  (modify-syntax-entry ?\"  "\"" ncl-mode-syntax-table)
  (modify-syntax-entry ?\\  "." ncl-mode-syntax-table)
  (modify-syntax-entry ?_   "w"  ncl-mode-syntax-table)
  (modify-syntax-entry ?{   "\(}"  ncl-mode-syntax-table)
  (modify-syntax-entry ?}   "\){"  ncl-mode-syntax-table)
  (modify-syntax-entry ?$   "_"  ncl-mode-syntax-table)
  (modify-syntax-entry ?.   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?\n  ">"  ncl-mode-syntax-table)
  (modify-syntax-entry ?\f  ">"  ncl-mode-syntax-table))

(defvar ncl-find-symbol-syntax-table nil
  "Syntax table that treats symbol characters as word characters.")

(if ncl-find-symbol-syntax-table ()
  (setq ncl-find-symbol-syntax-table
        (copy-syntax-table ncl-mode-syntax-table))
  )
;;****************************************************************************
;; keymap
;;****************************************************************************
(defvar ncl-mode-map nil
  "Keymap used in NCL mode.")
(if ncl-mode-map ()
  (setq ncl-mode-map (make-sparse-keymap))
  (define-key ncl-mode-map "\t"       'ncl-indent-line))
;;****************************************************************************
;; indenting variables
;;****************************************************************************
(defvar ncl-main-block-indent 2
  "*Extra indentation for the main block of code. That is the block between
the begin statement and the end statement.")

(defvar ncl-main-block-end -2
  "*The offset that places the end statement back on the left margin. This is
the negative of `ncl-main-block-indent`")

(defvar ncl-block-indent 2
  "*Extra indentation for do loops.")

(defvar ncl-block-end -2
  "*The offset that places the `end do` statement back to it origin.")

(defconst ncl-comment-line-start-skip "^[ \t]*;"
  "Regexp to match the start of a full-line comment. That is the
_beginning_ of a line containing a comment delmiter `\;' preceded
only by whitespace.")

;; defconst are constants that never change
;; the \` matches only those at the beginning of the buffer and no other
(defconst ncl-begin "\\<\\(begin\\)\\>\\|\\`"
  "Regular expression to find the begin statement.")

;; the \' matches only those at the end of the buffer and no other
(defconst ncl-end "\\<\\(^end$\\)\\>\\|\\'"
  "Regular expression to find the line that indicates the end of a
script.")

(defconst ncl-begin-do "^[ /t]*do"
  "Regular expression to find the beginning of a do loop.")

(defconst ncl-else "^[ /t]*else"
  "Regular expression to find an else statment.")

(defconst ncl-begin-if "^[ /t]*if"
  "Regular expression to find the beginning of a if statment.")

(defconst ncl-enddo "end[ ]do"
  "Regular expression to find the end of a do loop")

(defconst ncl-endif "end[ ]if"
  "Regular expression to find the end of a if statement")

(defconst ncl-identifier "[a-zA-Z][a-zA-Z0-9$_]+"
  "Regular expression matching an NCL identifier.")

(defconst ncl-label (concat ncl-identifier ":")
  "Regular expression matching NCL labels.")

(defvar ncl-no-change-comment ";;"
  "*The indentation of a comment that starts with this regular
expression will not be changed. Note that the indentation of a comment
at the beginning of a line is never changed.")

(defvar ncl-begin-line-comment nil
  "*A comment anchored at the beginning of line.
A comment matching this regular expression will not have its
indentation changed.  If nil the default is \"^\;\", i.e., any line
beginning with a \"\;\".  Expressions for comments at the beginning of
the line should begin with \"^\".")

(defvar ncl-code-comment ";;[^;]"
  "*A comment that starts with this regular expression on a line by
itself is indented as if it is a part of NCL code.  As a result if
the comment is not preceded by whitespace it is unchanged.")
;;****************************************************************************
;; indenting functions
;;****************************************************************************
(defun ncl-beginning-of-statement ()
  "Move to beginning of the current statement. Skips back past statement
continuations. Point is placed at the beginning of the line whether or not
this is an actual statement."
  (if (save-excursion (forward-line -1) (ncl-is-continuation-line))
      (ncl-previous-statement)
    (beginning-of-line)))

(defun ncl-end-of-statement ()
  "Moves point to the end of the current NCL statement. If not in a statement
just moves to end of line. Returns position."
  (interactive)
  (while (and (ncl-is-continuation-line)
              (= (forward-line 1) 0)))
  (end-of-line) (point))

(defun ncl-previous-statement ()
  "Moves point to beginning of the previous statement. Returns t if the
current line before moving is the beginning of the first non-comment
statement in the file, and nil otherwise."
  (interactive)
  (let (first-statement)
    (if (not (= (forward-line -1) 0))
        ;; first line in file
        t
      ;; skip blank lines, label lines, include lines and line comments
      (while (and
              ;; The current statement is the first statement until we
              ;; reach another statement.
              (setq first-statement
                    (or
                     (looking-at ncl-comment-line-start-skip)
                     (looking-at "[ \t]*$")
                     (looking-at (concat "[ \t]*" ncl-label "[ \t]*$"))
                     (looking-at "^@")))
              (= (forward-line -1) 0)))
      ;; skip continuation lines
      (while (and
              (save-excursion
                (forward-line -1)
                (ncl-is-continuation-line))
              (= (forward-line -1) 0)))
      first-statement)))

(defun ncl-is-continuation-line ()
  "Tests if current line is continuation line."
  (save-excursion
    (ncl-look-at "\\<\\$")))

(defun ncl-look-at (regexp &optional cont beg)
  "Searches current line from current point for the regular expression REGEXP.
If optional argument CONT is non-nil, searches to the end of the current
statement. If optional arg BEG is non-nil, search starts from the beginning
of the current statement. Ignores matches that end in a comment or inside a
string expression. Returns point if successful, nil otherwise.  This function
produces unexpected results if REGEXP contains quotes or a comment delimiter.
The search is case insensitive.  If successful leaves point after the match,
otherwise, does not move point."
  (let ((here (point))
        (old-syntax-table (syntax-table))
        (case-fold-search t)
        eos
        found)
    (set-syntax-table ncl-find-symbol-syntax-table)
    (setq eos
          (if cont
              (save-excursion (ncl-end-of-statement) (point))
            (save-excursion (end-of-line) (point))))
    (if beg (ncl-beginning-of-statement))
    (while (and (setq found (re-search-forward regexp eos t))
                (ncl-quoted)))
    (set-syntax-table old-syntax-table)
    (if (not found) (goto-char here))
    found))

(defun ncl-in-quote ()
  "Returns location of the opening quote if point is in a NCL string constant,
nil otherwise. Ignores comment delimiters on the current line. Properly
handles nested quotation marks and octal constants - a double quote followed
by an octal digit."
;;; Treat an octal inside an apostrophe to be a normal string. Treat a
;;; double quote followed by an octal digit to be an octal constant
;;; rather than a string. Therefore, there is no terminating double
;;; quote.
  (save-excursion
    ;; Because single and double quotes can quote each other we must
    ;; search for the string start from the beginning of line.
    (let* ((start (point))
           (eol (progn (end-of-line) (point)))
           (bq (progn (beginning-of-line) (point)))
           (endq (point))
           (data (match-data))
           delim
           found)
      (while  (< endq start)
        ;; Find string start
        ;; Don't find an octal constant beginning with a double quote
        (if (re-search-forward "\"[^0-7]\\|'\\|\"$" eol 'lim)
            ;; Find the string end. In NCL, two consecutive delimiters
            ;; after the start of a string act as an escape for the
            ;; delimiter in the string. Two consecutive delimiters alone
            ;; (i.e., not after the start of a string) is the the
            ;; null string.
            (progn
              ;; Move to position after quote
              (goto-char (1+ (match-beginning 0)))
              (setq bq (1- (point)))
              ;; Get the string delimiter
              (setq delim (char-to-string (preceding-char)))
              ;; Check for null string
              (if (looking-at delim)
                  (progn (setq endq (point)) (forward-char 1))
                ;; Look for next unpaired delimiter
                (setq found (search-forward delim eol 'lim))
                (while (looking-at delim)
                  (forward-char 1)
                  (setq found (search-forward delim eol 'lim)))
                (if found
                    (setq endq (- (point) 1))
                  (setq endq (point)))
                ))
          (progn (setq bq (point)) (setq endq (point)))))
      (store-match-data data)
      ;; return string beginning position or nil
      (if (> start bq) bq))))


(defun ncl-quoted ()
  "Returns t if point is in a comment or quoted string. nil otherwise."
  ;;  (or (ncl-in-comment) (ncl-in-quote)))
  (or (ncl-in-quote)))

(defun ncl-in-comment ()
  "Returns t if point is inside a comment, nil otherwise."
  (save-excursion
    (let ((here (point)))
      (and (ncl-goto-comment) (> here (point))))))

(defun ncl-goto-comment ()
  "Move to start of comment delimiter on current line. Moves to end of line if
there is no comment delimiter. Ignores comment delimiters in strings. Returns
point if comment found and nil otherwise."
  (let ((eos (progn (end-of-line) (point)))
        (data (match-data))
        found)
    ;; Look for first comment delimiter not in a string
    (beginning-of-line)
    (setq found (search-forward comment-start eos 'lim))
    (while (and found (ncl-in-quote))
      (setq found (search-forward comment-start eos 'lim)))
    (store-match-data data)
    (and found (not (ncl-in-quote))
         (progn
           (backward-char 1)
           (point)))))

(defun ncl-current-statement-indent ()
  "Return indentation of the current statement. If in a statement, moves to
beginning of statement before finding indent."
  (ncl-beginning-of-statement)
  (ncl-current-indent))

(defun ncl-current-indent ()
  "Return the column of the indentation of the current line.  Skips any
whitespace. Returns 0 if the end-of-line follows the whitespace."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    ;; if we are at the end of blank line return 0
    (cond ((eolp) 0)
          ((current-column)))))

(defun ncl-calculate-indent ()
  "Return appropriate indentation for current line as NCL code."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; if line is "begin" do nothing and exit
     ((ncl-look-at ncl-begin) 0)
     ;; calculate indent based on previous and current statements
     (t (let ((the-indent
               ;; calculate indent based on previous statement
               (save-excursion
                 (cond
                  ;; retreive the previous statement
                  ( (ncl-previous-statement) 0)

                  ;; indent if previous statment is begin
                  ((ncl-look-at ncl-begin t)
                   (+ (ncl-current-statement-indent) ncl-main-block-indent))

                  ;; indent if previous statment is do
                  ((ncl-look-at ncl-begin-do t)
                   (+ (ncl-current-statement-indent) ncl-block-indent))

                  ;; indent if previous statment is if
                  ((ncl-look-at ncl-begin-if t)
                   (+ (ncl-current-statement-indent) ncl-block-indent))

                  ;; indent if previous statment is else
                  ((ncl-look-at ncl-else t)
                   (+ (ncl-current-statement-indent) ncl-block-indent))

                  ((ncl-current-statement-indent))))))
          ;; adjust the indentation based on the current statement
          (cond
           ;; do loop
           ((ncl-look-at ncl-enddo t)
            (+ the-indent ncl-block-end))
           ;; if statement
           ((ncl-look-at ncl-endif t)
            (+ the-indent ncl-block-end))
           ;; else statement
           ((ncl-look-at ncl-else t)
            (+ the-indent ncl-block-end))

           ;; End block
           ((ncl-look-at ncl-end t)
            (+ the-indent ncl-main-block-end)) ;; end gets negative indent
           (the-indent))

          )))))

(defun ncl-indent-to (col &optional min)
  "Indent from point with spaces until column COL. Inserts space before
markers at point."
  (if (not min) (setq min 0))
  (insert-before-markers
   (make-string (max min (- col (current-column))) ? )))

(defun ncl-indent-left-margin (col)
  "Indent the current line to column COL. Indents such that first
non-whitespace character is at column COL. Inserts spaces before markers at
point."
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (ncl-indent-to col)))

(defun ncl-comment-hook ()
  "Compute indent for the beginning of the NCL comment delimiter."
  (if (or (looking-at ncl-no-change-comment)
          (if ncl-begin-line-comment
              (looking-at ncl-begin-line-comment)
            (looking-at "^\;")))
      (current-column)
    (if (looking-at ncl-code-comment)
        (if (save-excursion (skip-chars-backward " \t") (bolp))
            ;; On line by itself, indent as code
            (let ((tem (ncl-calculate-indent)))
              (if (listp tem) (car tem) tem))
          ;; after code - do not change
          (current-column))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defun ncl-indent-line ()
  "Indents current NCL line as code or as a comment."
  (interactive)
  ;; Move point out of left margin.
  (if (save-excursion
        (skip-chars-backward " \t")
        (bolp))
      (skip-chars-forward " \t"))
  (let ((mloc (point-marker)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at ncl-comment-line-start-skip)
          ;; Indentation for a line comment
          (progn
            (skip-chars-forward " \t")
            (ncl-indent-left-margin (ncl-comment-hook)))
        ;; Indent for code line
        (beginning-of-line)
        (if (or
             ;; a label line
             (looking-at (concat "^" ncl-label "[ \t]*$"))
             ;; a batch command
             (looking-at "^[ \t]*@"))
            ;; leave flush left
            nil
          ;; indent the line
          (ncl-indent-left-margin (ncl-calculate-indent)))
        ;; Adjust parallel comment
        ;;        (end-of-line)
        ;;        (if (ncl-in-comment)
        ;;            (indent-for-comment))
        ))
    (goto-char mloc)
    ;; Get rid of marker
    (set-marker mloc nil)
    ))

;;****************************************************************************
;; the command to comment/uncomment text
;;****************************************************************************
(defun ncl-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start ";") (comment-end ""))
    (comment-dwim arg)))

;;****************************************************************************
;; define ncl mode
;;****************************************************************************
(defun ncl-mode ()
  "Major mode for editing NCL .ncl files"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ncl-mode)
  (setq mode-name "NCL")

  ;; modify the keymap
  (define-key ncl-mode-map [remap comment-dwim] 'ncl-comment-dwim)

  (if ncl-startup-message
      (message "Emacs NCL mode version %s." ncl-mode-version)
    )
  ;;**************************
  ;; indentation
  ;;**************************
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ncl-indent-line)
  (use-local-map ncl-mode-map)
  ;;**************************
  ;; these ensure syntax hightlighting
  ;;**************************
  ;; font-lock setup for various emacs: XEmacs, Emacs 19.29+, Emacs <19.29.
  ;; taken from html-helper-mode, adapted to ncl
  (cond ((string-match "XEmacs\\|Lucid" (emacs-version)) ; XEmacs/Lucid
         (put major-mode 'font-lock-keywords-case-fold-search t)
         (put major-mode 'font-lock-syntactic-keywords t)
         (put major-mode 'font-lock-maximum-decoration 2)
         )
        ;; not sure if this is correct
        ;; XEmacs (19.13, at least) guesses the rest correctly.
        ;; If any older XEmacs don't, then tell me.
        ;;
        ((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
         (make-local-variable 'font-lock-defaults)
         (setq font-lock-defaults '(ncl-font-lock-keywords nil t)))
        ;;
        (t ; Emacs 19.28 and older
         (make-local-variable 'font-lock-keywords-case-fold-search)
         (make-local-variable 'font-lock-keywords)
         ;;(make-local-variable 'font-lock-no-comments)
         (setq font-lock-keywords-case-fold-search t)
         (setq font-lock-keywords ncl-font-lock-keywords)
         ;;(setq font-lock-no-comments t)
         ))

  ;; imenu thing
  (set (make-local-variable 'imenu-generic-expression)
       ncl-imenu-generic-expression)

  (font-lock-mode 1)
  ;;  (setq font-lock-maximum-decoration t)
  ;;  (make-local-variable 'font-lock-defaults)
  ;;  (setq font-lock-defaults 'ncl-keywords)
  ;;  (make-local-variable 'comment-start)
  ;;  (setq comment-start ";")
  ;; turn this on if debuging this code
  (setq debug_on_error t)
  (set-syntax-table ncl-mode-syntax-table)
  (run-hooks 'ncl-mode-hook))


;;************************************************************************
(provide 'ncl)
;;; ncl.el ends here
