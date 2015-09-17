
Build a packer file: 

    buildlab -baseConfigName windows_81_x86 -action BuildPacker -tag "TestBuildForFun"

Add it to Vagrant

    buildlab -baseConfigName windows_81_x86 -action AddToVagrant -tag "TestBuildForFun"

You can do both of these at once with 

    -action BuildPacker,AddToVagrant

More shit to come later
