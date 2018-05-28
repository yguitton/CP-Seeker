tabItem(tabName = 'Files',
	box(width=6, title='Import files',
		div(style='text-align:center;',
			column(width=6, shinyFilesButton('fileImportmzXML', label='Import mzXML(s)', 
				'select mzXML(s) or mzXML(s) ', multiple=TRUE, 
				class="bttn-unite bttn-md bttn-default bttn-bttn-no-outline", icon=icon('upload'))),
			column(width=6, shinyFilesButton('fileImportRaw', label='Import raw(s)', 
				'select raw(s)', multiple=TRUE, 
				class="bttn-unite bttn-md bttn-default bttn-bttn-no-outline", icon=icon('upload')))
		)
	),
	box(width=6, title='Informations',
		uiOutput('uiFileInfo'),
		dataTableOutput("tableInfoConversion")
	)
)