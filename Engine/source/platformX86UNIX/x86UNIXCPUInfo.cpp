//-----------------------------------------------------------------------------
// Copyright (c) 2012 GarageGames, LLC
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
//-----------------------------------------------------------------------------

#include "platform/platform.h"
#include "platformX86UNIX/platformX86UNIX.h"
#include "console/console.h"
#include "core/stringTable.h"
#include "core/strings/stringFunctions.h"
#include <math.h>

#include <hwloc.h>
#include <hwloc/cpukinds.h>

#include "platform/platformCPUCount.h"
#include <unistd.h>

Platform::SystemInfo_struct Platform::SystemInfo;

#if defined(TORQUE_CPU_X64) || defined(TORQUE_CPU_X32)
#include <cpuid.h>

String getCPUManufacturerID()
{
    U32 ignored;
    U32 result[3];
    memset(result, 0x00, sizeof(result) / sizeof(U32));

    if (!__get_cpuid(0, &ignored, &result[0], &result[2], &result[1]))
    {
        return "<Error Retrieving Manufacturer ID>";
    }

    return (char*)result;
}

String getCPUBrandString()
{
    // Leaves 0x8000002 to 0x8000004 are used for brand string
    U32 result[16];
    memset(result, 0x00, sizeof(result) / sizeof(U32));

    const U32 leafBase = 0x80000002;
    for (U32 iteration = 0; iteration < 3; ++iteration)
    {
        // We load 4 U32's per iteration
        const U32 indexBase = iteration * 4;
        if (!__get_cpuid(leafBase + iteration, &result[indexBase], &result[indexBase + 1], &result[indexBase + 2], &result[indexBase + 3]))
        {
            return "<Error Retrieving Brand String>";
        }
    }

    return (char*)result;
}

U32 getCPUCapabilities()
{
    U32 eax = 0;
    U32 ebx = 0;
    U32 ecx = 0;
    U32 edx = 0;
    if (!__get_cpuid(0x1, &eax, &ebx, &ecx, &edx))
    {
        return 0;
    }

    // Once all hardware is loaded, set our capabilities
    U32 result = 0;

    // EDX register
    if (edx & bit_MMX)
    {
        result |= CPU_PROP_MMX;
    }
    if (edx & bit_3DNOW)
    {
        result |= CPU_PROP_3DNOW;
    }
    if (edx & bit_SSE)
    {
        result |= CPU_PROP_SSE;
    }
    if (edx & bit_SSE2)
    {
        result |= CPU_PROP_SSE2;
    }

    // ECX register
    if (ecx & bit_SSE3)
    {
        result |= CPU_PROP_SSE3;
    }
    if (ecx & bit_SSSE3)
    {
        result |= CPU_PROP_SSE3ex;
    }
    if (ecx & bit_SSE4_1)
    {
        result |= CPU_PROP_SSE4_1;
    }
    if (ecx & bit_SSE4_2)
    {
        result |= CPU_PROP_SSE4_2;
    }
    if (ecx & bit_AVX)
    {
        result |= CPU_PROP_AVX;
    }

    return result;
}
#endif

bool getCPUTopology(U32& physicalCores, U32& logicalCores)
{
    hwloc_topology_t cpuTopology;
    if (hwloc_topology_init(&cpuTopology) == 0 && hwloc_topology_load(cpuTopology) == 0)
    {
        physicalCores = hwloc_get_nbobjs_by_type(cpuTopology, HWLOC_OBJ_CORE);
        logicalCores = hwloc_get_nbobjs_by_type(cpuTopology, HWLOC_OBJ_PU);

        hwloc_topology_destroy(cpuTopology);

        return true;
    }

    return false;
}

bool getCPUFrequencies(U32& baseFrequency, U32& maxFrequency)
{
    maxFrequency = 0;
    baseFrequency = 0;

    hwloc_topology_t cpuTopology;
    if (hwloc_topology_init(&cpuTopology) == 0 && hwloc_topology_load(cpuTopology) == 0)
    {
        // NOTE: There should be at least 1 kind here - on end user machines this should be 1
        // but in case we have a multi-socket system we'll take the first entry
        S32 kindCount = hwloc_cpukinds_get_nr(cpuTopology, 0);
        if (kindCount > 0)
        {
            // hwloc_bitmap_s bitmap;
            U32 informationCount = 0;
            hwloc_info_s* information;
            if(hwloc_cpukinds_get_info(cpuTopology, 0, NULL, NULL, &informationCount, &information,0) == 0)
            {
                // Within our information array we have an array of attributes so we locate FrequencyMaxMHz or FrequencyBaseMHz
                for (U32 iteration = 0; iteration < informationCount; ++iteration)
                {
                    if (dStrcmp(information[iteration].name, "FrequencyMaxMHz") == 0)
                    {
                        maxFrequency = dAtoui(information[iteration].value);
                    }

                    if (dStrcmp(information[iteration].name, "FrequencyBaseMHz") == 0)
                    {
                        baseFrequency = dAtoui(information[iteration].value);
                    }
                }
            }
        }

        return baseFrequency && maxFrequency == 0;
    }

    return false;
}

void Processor::init()
{
#if defined(TORQUE_CPU_X64) || defined(TORQUE_CPU_X32)
    // Set sane default information
    Platform::SystemInfo.processor.type = CPU_X86Compatible;
    Platform::SystemInfo.processor.name = StringTable->insert(getCPUBrandString().trim());
    Platform::SystemInfo.processor.properties = CPU_PROP_C | CPU_PROP_FPU | CPU_PROP_LE | getCPUCapabilities();

    // Determine CPU manufacturer
    const String manufacturer = getCPUManufacturerID();
    if (manufacturer.compare("AuthenticAMD") == 0)
    {
        Platform::SystemInfo.processor.type = CPU_AMD;
    }
    else if (manufacturer.compare("GenuineIntel") == 0)
    {
        Platform::SystemInfo.processor.type = CPU_Intel;
    }

#elif defined(TORQUE_CPU_ARM32) || defined(TORQUE_CPU_ARM64)
    Platform::SystemInfo.processor.type = CPU_ArmCompatible;
    Platform::SystemInfo.processor.name = StringTable->insert("Unknown ARM Processor");
    Platform::SystemInfo.processor.properties = CPU_PROP_C;
#else
#warning Unsupported CPU
#endif

#if defined(TORQUE_CPU_X64) || defined(TORQUE_CPU_ARM64)
    Platform::SystemInfo.processor.properties |= CPU_PROP_64bit;
#endif

    // Retrieve CPU topology
    getCPUTopology(Platform::SystemInfo.processor.numPhysicalProcessors, Platform::SystemInfo.processor.numLogicalProcessors);
    Platform::SystemInfo.processor.isHyperThreaded = Platform::SystemInfo.processor.numLogicalProcessors != Platform::SystemInfo.processor.numPhysicalProcessors;
    Platform::SystemInfo.processor.isMultiCore = Platform::SystemInfo.processor.numPhysicalProcessors != 1;

    // Load frequency information - if base isn't present we use turbo
    U32 maxFrequency = 0;
    getCPUFrequencies(Platform::SystemInfo.processor.mhz, maxFrequency);

    if (Platform::SystemInfo.processor.mhz == 0)
    {
        Platform::SystemInfo.processor.mhz = maxFrequency;
    }
}

namespace CPUInfo
{
    EConfig CPUCount(U32& TotAvailLogical, U32& PhysicalNum)
    {
        TotAvailLogical = Platform::SystemInfo.processor.numLogicalProcessors;
        PhysicalNum = Platform::SystemInfo.processor.numPhysicalProcessors;

        const bool hyperThreaded = TotAvailLogical != PhysicalNum;
        if (PhysicalNum != 1)
        {
            return hyperThreaded ? CONFIG_MultiCoreAndHTEnabled : CONFIG_MultiCoreAndHTNotCapable;
        }
        else
        {
            return hyperThreaded ? CONFIG_SingleCoreHTEnabled : CONFIG_SingleCoreAndHTNotCapable;
        }
    }
}; // namespace CPUInfo
