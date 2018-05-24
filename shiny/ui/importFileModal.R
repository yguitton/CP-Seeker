tagList(bsModal(id='fileProjectmzXMLModal', title='Choose a project', trigger='fileChoosemzXMLModal', close.button=FALSE, 
	uiOutput('uiFileProjectmzXML'),
	footer=actionBttn('fileChoosemzXML', 'Valid', style='material-flat', color='primary', size='sm')
),
	
bsModal(id='fileProjectRawModal', title='Choose a project', trigger='fileChooseRawModal', close.button=FALSE, 
	uiOutput('uiFileProjectRaw'),
	footer=actionBttn('fileChooseRaw', 'Valid', style='material-flat', color='primary', size='sm')
))
	