add_library(ndtreefortran
	NdTreeFortran.f90
	ndnode/NdNodeDistance.f90
	ndnode/NdNodeGetters.f90
	ndnode/NdNodeUtils.f90
	ndtree/NdTreeGetters.f90
	ndtree/NdTreeModders.f90
	ndtree/NdTreeUtils.f90
	kdtree/KdTreeBuild.f90
	kdtree/KdTreeUtils.f90
	kdtree/KdTreeRnn.f90
	kdtree/KdTreeModders.f90
	ndtree/search_modules/NdTreeRnn.f90
	ndtree/search_modules/NdTreeLinScan.f90
	ndtree/search_modules/NdTreeDBSCAN.f90
)

target_include_directories(ndtreefortran PUBLIC
	$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(ndtreefortran PRIVATE OpenMP::OpenMP_Fortran)
