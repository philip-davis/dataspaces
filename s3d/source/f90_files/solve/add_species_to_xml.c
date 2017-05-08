#include <mxml.h>
#include <stdio.h>

void add_species_to_xml_(int * L, const char * spcname){


    FILE *fp;
    mxml_node_t *xmlfile;
    mxml_node_t *node;

    if( *L == 1 ){
	printf("Adding species %d (%s) to xml template file... \n",*L, spcname);
	fp = fopen("s3d_template.xml", "r");
    }
    else{
	printf("Adding species %d (%s) to xml file... \n",*L, spcname);
	fp = fopen("s3d.xml", "r");
    }

    xmlfile = mxmlLoadFile( NULL, fp, MXML_TEXT_CALLBACK);
    fclose(fp);


    node = mxmlFindElement( xmlfile, xmlfile, "global-bounds", NULL, NULL, MXML_DESCEND);

    mxml_node_t *newnode;

    newnode = mxmlNewElement( node, "var");
    mxmlElementSetAttr( newnode, "name", spcname);
    mxmlElementSetAttr( newnode, "type", "double");
    mxmlElementSetAttr( newnode, "dimensions", "nz,ny,nx");


    fp = fopen("s3d.xml", "w");
    mxmlSaveFile( xmlfile, fp, MXML_NO_CALLBACK);
    fclose(fp);


}
