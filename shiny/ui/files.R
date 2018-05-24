tabItem(tabName = 'Files',
	box(width=6, title='Import files',
		column(width=3, shinyFilesButton('fileImportmzXML', label='Import mzXML(s)', 'select mzXML(s) or mzXML(s) ', multiple=TRUE)),
		column(width=3, shinyFilesButton('fileImportRaw', label='Import raw(s)', 'select raw(s)', multiple=TRUE))
	),
	box(width=6, 'Informations',
		uiOutput('uiFileInfo'),
		dataTableOutput("tableInfoConversion")
	)
)