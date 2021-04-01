shinydashboard::tabItem(tabName = 'process', 
	shiny::column(id = "tab_process", width = 3, shinydashboard::box(width = 12, 
		shiny::column(width = 12, style = "margin-bottom: 20px; text-align: center;", 
			shinyWidgets::actionBttn('process_launch', 'Launch deconvolution process', 
				style = 'minimal', color = 'primary')
		),
		shiny::tabsetPanel(
			shiny::tabPanel(title = shinyWidgets::awesomeCheckbox("process_xcms_lock", 
					label = shiny::tags$b("Peak picking"), TRUE), 
				shiny::tags$br(),
				shiny::tabsetPanel(
					shiny::tabPanel('General', 
						shiny::tags$br(),
						shiny::tags$table(class = "table-params", 
							shiny::tags$tr(
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput('process_ppm', 
											'm/z tolerance (ppm)', value = 5),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Maximal tolerated m/z deviation in consecutive scans in parts per million (ppm)'
										)
									)
								),
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput("process_snthresh", "s/n", 
											value = 10),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Signal to noise ratio cutoff'
										)
									)
								)
							), 
							shiny::tags$tr(
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput('process_peakwidth_min', 
											'Peakwidth min (s)', value = 5),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Expected approximate peak width in chromatographic space'
										)
									)
								),
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput('process_peakwidth_max', 
											'Peakwidth max (s)', value = 60),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Expected approximate peak width in chromatographic space'
										)
									)
								)
							),
							shiny::tags$tr(
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput('process_prefilter_step', 
											'Prefilter step', value = 3),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Mass traces are only retained if they contain at least k peaks with intensity >= I'
										)
									)
								),
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput('process_prefilter_level', 
											'Prefilter level', value = 10000),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Mass traces are only retained if they contain at least k peaks with intensity >= I'
										)
									)
								)
							)
						)
					),
					shiny::tabPanel('Advanced', 
						shiny::tags$br(),
						shiny::tags$table(class = "table-params", 
							shiny::tags$tr(
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::selectInput('process_mz_center_fun', 
											'm/z center function', 
											choices = c('wMean', 'mean', 'apex', 
												'wMeanApex3', 'meanApex3'), 
											selected = 'wMean'),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Name of the function to calculate the m/z center of the chromatographic peak'
										)
									)
								),
								shiny::tags$td()
							),
							shiny::tags$tr(
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shinyWidgets::awesomeCheckbox(
											'process_first_baseline_check', 
											tags$b('Baseline check'), TRUE),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Continuous data within regions of interest is checked to be above the first baseline'
										)
									)
								),
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shinyWidgets::awesomeCheckbox("process_integrate", 
											tags$b("Integration by CWT"), TRUE),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Integration method. If checked the descent is done on the real data, if not peak limits are found through descent on the mexican hat filtered data.  Method 1 is very accurate but prone to noise,  while method 2 is more robust to noise but less exact'
										)
									)
								)
							),
							shiny::tags$tr(
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput("process_noise", 
											label = "Noise", value = 0),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Optional argument which is useful for data that was centroided without any intensity threshold, centroids with intensity < noise are omitted from ROI detection'
										)
									)
								),
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput("process_mzdiff", 
											label = 'm/z difference', value = 0.001),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Minimum difference in m/z for peaks with overlapping retention times, can be negative to allow overlap'
										)
									)
								)
							)
						)
					)
				)
			),
			shiny::tabPanel(title = shinyWidgets::awesomeCheckbox("process_pairing_lock", 
					label = tags$b("Pairing"), TRUE), 
				tags$br(),
				shiny::tags$table(class = "table-params", 
					shiny::tags$tr(
						shiny::tags$td(
							bsplus::shinyInput_label_embed(
								shiny::numericInput("process_rt_tol", 
									label = "tR tolerance (s)", value = 1, step=.1),
								bsplus::bs_embed_tooltip(
									bsplus::shiny_iconlink(),
									placement = 'top', 
									title = "Retention time tolerance to pair features into halogen-containing clusters"
								)
							)
						), 
						shiny::tags$td(
							bsplus::shinyInput_label_embed(
								shiny::numericInput("process_mz_tol", 
									label = 'm/z tolerance (mDa)', value=0.5, step=.1),
								bsplus::bs_embed_tooltip(
									bsplus::shiny_iconlink(),
									placement = 'top', 
									title = "m/z tolerance to pair features into halogen-containing clusters"
								)
							)
						)
					)
				)
			),
			shiny::tabPanel(title = shinyWidgets::awesomeCheckbox('process_alignment_lock', 
					label = shiny::tags$b('Alignment'), TRUE), 
				shiny::tags$br(),
				shiny::tabsetPanel(
					shiny::tabPanel('General', 
						shiny::tags$br(),
						bsplus::shinyInput_label_embed(
							shiny::selectInput("process_center_sample", 
								"Select the referent sample", 
								choices = c("Most informative")),
							bsplus::bs_embed_tooltip(
								bsplus::shiny_iconlink(),
								placement = 'top', 
								title = 'Sample used as sample of reference to apply the retention time correction'
							)
						),
						shiny::fluidRow(
							shiny::column(width = 6, 
								bsplus::shinyInput_label_embed(
									shiny::numericInput('process_bw', 'tR tolerance', 
										value = 1),
									bsplus::bs_embed_tooltip(
										bsplus::shiny_iconlink(),
										placement = 'top', 
										title = 'retention time window tolerance (s)'
									)
								)
							), 
							shiny::column(width = 6, 
								bsplus::shinyInput_label_embed(
									shiny::numericInput('process_mzwid', 
										'm/z tolerance (mDa) between samples', value = 1),
									bsplus::bs_embed_tooltip(
										bsplus::shiny_iconlink(),
										placement = 'top', 
										title = 'm/z window tolerance (mDa)'
									)
								)
							)
						)
					),
					shiny::tabPanel('Advanced',
						shiny::tags$br(),
						bsplus::shinyInput_label_embed(
							shiny::selectInput('process_dist_fun', 'Distance function', 
								choices = c('cor', 'cor_opt', 'cov', 'prd', 'euc'), 
								selected = 'cor_opt'),
							bsplus::bs_embed_tooltip(
								bsplus::shiny_iconlink(),
								placement = 'top', 
								title = 'Distance function to be used.  Allowed values are "cor"(Pearson\'s correlation), "cor_opt"(calculate only 10% diagonal band of distance matrix; better runtime), "cov"(covariance), "prd"(product) and "euc"(Euclidian distance)'
							)
						),
						bsplus::shinyInput_label_embed(
							shiny::sliderInput('process_response', 'Response', 
								min = 0, max = 100, value = 1, step = 1),
							bsplus::bs_embed_tooltip(
								bsplus::shiny_iconlink(),
								placement = 'top', 
								title = 'Defining the responsiveness of warping with response = 0 giving linear warping on start and end points and response = 100 warping using all bijective anchors.'
							)
						),
						shiny::tags$table(class = "table-params", 
							shiny::tags$tr(
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput('process_gap_init', "Gap init", 
											value = .3),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Defining the penalty for gap opening'
										)
									)
								),
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput('process_gap_extend', 
											"Gap extend", value = 2.4),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Defining the penalty for gap enlargement'
										)
									)
								)
							),
							shiny::tags$tr(
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput('process_factor_diag', 
											'Factor diagonal', value = 2),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Defining the local weight applied to diagonal moves in the alignment'
										)
									)
								),
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput('process_factor_gap', 
											'Factor gap', value = 1),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Defining the local weight for gap moves in the alignment'
										)
									)
								)
							),
							shiny::tags$tr(
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shiny::numericInput('process_init_penalty', 
											"Initiating penalty", value = 0),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Defining the penalty for initiating an alignment (for local alignmentonly)'
										)
									)
								),
								shiny::tags$td(
									bsplus::shinyInput_label_embed(
										shinyWidgets::awesomeCheckbox(
											'process_local_alignment', 
											shiny::tags$b('Local alignment'), FALSE),
										bsplus::bs_embed_tooltip(
											bsplus::shiny_iconlink(),
											placement = 'top', 
											title = 'Whether a local alignment should be performed instead of the default global alignment'
										)
									)
								)								
							)
						)
					)
				)
			),
			shiny::tabPanel(title = shinyWidgets::awesomeCheckbox('process_camera_lock', 
					label = shiny::tags$b('Fragment/adduct'), TRUE), 
				shiny::tags$br(), 
				bsplus::shinyInput_label_embed(
					shiny::sliderInput('process_perfwhm', 'Percentage of the width at FWHM', 
						value = 60, min = 0, max = 100),
					bsplus::bs_embed_tooltip(
						bsplus::shiny_iconlink(),
						placement = 'top', 
						title = 'Full Width at Half Maximum for grouping integrated ions'
					)
				),
				bsplus::shinyInput_label_embed(
					shiny::sliderInput('process_cor_eic_th', 
						'Threshold for EIC correlation (R2)', value = .75, min = 0, max = 1),
					bsplus::bs_embed_tooltip(
						bsplus::shiny_iconlink(),
						placement = 'top', 
						title = 'Threshold for Extracted Ion Chromatogram correlation'
					)
				),
				bsplus::shinyInput_label_embed(
					shiny::selectInput('process_graphMethod', 'Method for grouping peaks', 
						choices = c('hcs', 'lcs')),
					bsplus::bs_embed_tooltip(
						bsplus::shiny_iconlink(),
						placement = 'top', 
						title = 'Method used for grouping peaks. HCS means for Highly Connected Substract and LCS for Label Propagation Community. See package CAMERA for more informations.'
					)
				),
				shiny::tags$table(class = "table-params", 
					shiny::tags$tr(
						shiny::tags$td(
							bsplus::shinyInput_label_embed(
								shiny::numericInput('process_sigma', 
									'Mutliplier of the standard deviation', value = 6),
								bsplus::bs_embed_tooltip(
									bsplus::shiny_iconlink(),
									placement = 'top', 
									title = 'Multiplier of the standard deviation'
								)
							)
						), 
						shiny::tags$td(
							bsplus::shinyInput_label_embed(
								shiny::numericInput('process_pval', 'P-value threshold', 
									value = .05),
								bsplus::bs_embed_tooltip(
									bsplus::shiny_iconlink(),
									placement = 'top', 
									title = paste('Threshold for ', tags$i('P'), '-value', sep = "")
								)
							)
						)
					)
				)
			)
		)
	)),
	
	shinydashboard::box(width = 9, 
		shinycssloaders::withSpinner(plotly::plotlyOutput('process_TIC')),
		tags$hr(),
		shinycssloaders::withSpinner(plotly::plotlyOutput('process_MS'))
	)
)
