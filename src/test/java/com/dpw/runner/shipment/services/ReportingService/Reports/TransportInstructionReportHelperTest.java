package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsContainersModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsPackagesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsReferenceModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsTruckDriverModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.enums.TILegType;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TiContainers;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TiPackages;
import com.dpw.runner.shipment.services.entity.TiReferences;
import com.dpw.runner.shipment.services.entity.TiTruckDriverDetails;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TransportInstructionReportHelperTest extends CommonMocks {

    @InjectMocks
    private TransportInstructionReportHelper transportInstructionReportHelper;

    @Mock
    private ModelMapper modelMapper;


    @BeforeAll
    static void init() throws IOException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void testAddTransportInstructionLegsDataIntoDictionary() {
        TiLegs legs = new TiLegs();
        legs.setLegType(TILegType.EMPTY);
        Parties origin = new Parties();
        origin.setOrgData(Map.of("full_name", "Origin Name"));
        legs.setOrigin(origin);
        Parties destination = new Parties();
        destination.setOrgData(Map.of("full_name", "Destination Name"));
        legs.setDestination(destination);

        TILegsModel legsModel = new TILegsModel();
        when(modelMapper.map(any(TiLegs.class), eq(TILegsModel.class))).thenReturn(legsModel);
        when(modelMapper.map(any(Parties.class), eq(PartiesModel.class)))
                .thenReturn(new PartiesModel());

        Map<String, Object> legsDictionary = new HashMap<>();

        transportInstructionReportHelper.addTransportInstructionLegsDataIntoDictionary(legs, legsDictionary);

        assertEquals(true, legsDictionary.get(ReportConstants.HAS_LEGS));
        assertSame(legsModel, legsDictionary.get(ReportConstants.TI_LEGS));
    }

    @Test
    void testAddTransportInstructionLegsTruckDriverDataIntoDictionary_WhenListNotEmpty() {
        TiLegs legs = new TiLegs();
        TiTruckDriverDetails details = new TiTruckDriverDetails();
        legs.setTiTruckDriverDetails(List.of(details));

        TILegsTruckDriverModel model = new TILegsTruckDriverModel();
        when(modelMapper.map(any(TiTruckDriverDetails.class), eq(TILegsTruckDriverModel.class)))
                .thenReturn(model);

        Map<String, Object> legsDictionary = new HashMap<>();

        transportInstructionReportHelper.addTransportInstructionLegsTruckDriverDataIntoDictionary(legs, legsDictionary);

        assertEquals(true, legsDictionary.get(ReportConstants.HAS_TRUCK_DRIVERS));
    }

    @Test
    void testAddTransportInstructionLegsReferencesDataIntoDictionary_WhenListEmpty() {
        TiLegs legs = new TiLegs();
        TiReferences tiReferences = new TiReferences();
        legs.setTiReferences(List.of(tiReferences));


        TILegsReferenceModel model = new TILegsReferenceModel();
        when(modelMapper.map(any(TiReferences.class), eq(TILegsReferenceModel.class)))
                .thenReturn(model);

        Map<String, Object> legsDictionary = new HashMap<>();

        transportInstructionReportHelper.addTransportInstructionLegsReferencesDataIntoDictionary(legs, legsDictionary);

        assertTrue(legsDictionary.containsKey(ReportConstants.HAS_REFERENCE_DETAILS));
    }

    @Test
    void testAddTransportInstructionLegsPackagesDataIntoDictionary_WhenListNotEmpty() {
        TiLegs legs = new TiLegs();
        TiPackages packages = new TiPackages();
        legs.setTiPackages(List.of(packages));

        TILegsPackagesModel packagesModel = new TILegsPackagesModel();
        when(modelMapper.map(any(TiPackages.class), eq(TILegsPackagesModel.class)))
                .thenReturn(packagesModel);

        Map<String, Object> legsDictionary = new HashMap<>();

        transportInstructionReportHelper.addTransportInstructionLegsPackagesDataIntoDictionary(legs, legsDictionary);

        assertEquals(true, legsDictionary.get(ReportConstants.HAS_PACKAGE_DETAILS));
    }

    @Test
    void testAddTransportInstructionLegsContainersDataIntoDictionary_WhenListNotEmpty() {
        TiLegs legs = new TiLegs();
        TiContainers containers = new TiContainers();
        legs.setTiContainers(List.of(containers));

        TILegsContainersModel containersModel = new TILegsContainersModel();
        when(modelMapper.map(any(TiContainers.class), eq(TILegsContainersModel.class)))
                .thenReturn(containersModel);

        Map<String, Object> legsDictionary = new HashMap<>();

        transportInstructionReportHelper.addTransportInstructionLegsContainersDataIntoDictionary(legs, legsDictionary);

        assertEquals(true, legsDictionary.get(ReportConstants.HAS_CONTAINERS));
    }
}
