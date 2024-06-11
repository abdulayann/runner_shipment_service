package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class MawbReportTest {

    @InjectMocks
    private MawbReport mawbReport;

    @Mock
    private HawbReport hawbReport;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IAwbRepository awbRepository;

    @Mock
    private IAwbService awbService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    ShipmentDetails shipmentDetails;

    @BeforeEach
    void setup() {
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).DPWDateFormat("yyyy-MM-dd").build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder().airDGFlag(true).build());
        UsersDto usersDto = new UsersDto();
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.airDG, true);
        usersDto.setPermissions(permissions);
        UserContext.setUser(usersDto);
    }

    @Test
    void getDocumentModel() {
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(awbRepository.findByConsolidationId(any())).thenReturn(Arrays.asList(new Awb()));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationModel.setHazardous(true);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mawbReport.isDMawb = false;
        Assertions.assertNotNull(mawbReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_dgError() {
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationModel.setHazardous(true);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mawbReport.isDMawb = false;
        UserContext.getUser().setPermissions(new HashMap<>());
        Assertions.assertThrows(ValidationException.class, () -> mawbReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModelDMawb() {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        when(awbRepository.findByConsolidationId(any())).thenReturn(null);
        mawbReport.isDMawb = true;
        Assertions.assertNotNull(mawbReport.getDocumentModel(123L));
    }

    @Test
    void populateDictionary() {
        Assertions.assertNotNull(mawbReport.populateDictionary(null));
    }
}
