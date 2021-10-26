# This module loads in ShaderConductor, a project from Microsoft that translates HLSL code
# into other shading languages for other APIs

# TODO: Auto detect if using any other api besides D3D to automatically turn it on
option(TORQUE_SHADER_CONDUCTOR "Use ShaderConductor to convert HLSL into other shading languages" OFF)

if (NOT TORQUE_SHADER_CONDUCTOR)
    message("ShaderConductor is disabled, no runtime conversion of HLSL will happen")
endif()

# For now we will set shader conductor's path.
# Note that we will just make it download the artifact using cmake
# in the future for the DLL that we publish, so
# that users don't have to grab it.
# For now, just assume the user to just have it on hand while we develop this feature

if ("${SHADER_CONDUCTOR_PATH}" STREQUAL "")
    set(SHADER_CONDUCTOR_PATH "" CACHE PATH "ShaderConductor Path" FORCE)
endif()
if ("${SHADER_CONDUCTOR_PATH}" STREQUAL "")
    message(FATAL_ERROR "No ShaderConductor Path specified. Update this before continuing!")
    return()
endif()

if (MSVC)
    set(SHADER_CONDUCTOR_EXT ".dll")
elseif (APPLE)
    set(SHADER_CONDUCTOR_EXT ".dylib")
elseif (UNIX)
    set(SHADER_CONDUCTOR_EXT ".so")
else()
    message(FATAL_ERROR "ShaderConductor is currently not supported on this system")
    return()
endif()

addDef("TORQUE_SHADER_CONDUCTOR_ENABLED")
addPath("${srcDir}/gfx/shaderConductor")
addInclude("${SHADER_CONDUCTOR_PATH}/Include")

INSTALL(FILES "${SHADER_CONDUCTOR_PATH}/Bin/Debug/dxcompiler${SHADER_CONDUCTOR_EXT}" DESTINATION "${projectOutDir}" CONFIGURATIONS Debug)
INSTALL(FILES "${SHADER_CONDUCTOR_PATH}/Bin/Debug/ShaderConductor${SHADER_CONDUCTOR_EXT}" DESTINATION "${projectOutDir}" CONFIGURATIONS Debug)
INSTALL(FILES "${SHADER_CONDUCTOR_PATH}/Bin/Debug/ShaderConductorWrapper${SHADER_CONDUCTOR_EXT}" DESTINATION "${projectOutDir}" CONFIGURATIONS Debug)


INSTALL(FILES "${SHADER_CONDUCTOR_PATH}/Bin/Release/dxcompiler${SHADER_CONDUCTOR_EXT}" DESTINATION "${projectOutDir}" CONFIGURATIONS Release)
INSTALL(FILES "${SHADER_CONDUCTOR_PATH}/Bin/Release/ShaderConductor${SHADER_CONDUCTOR_EXT}" DESTINATION "${projectOutDir}" CONFIGURATIONS Release)
INSTALL(FILES "${SHADER_CONDUCTOR_PATH}/Bin/Release/ShaderConductorWrapper${SHADER_CONDUCTOR_EXT}" DESTINATION "${projectOutDir}" CONFIGURATIONS Release)
