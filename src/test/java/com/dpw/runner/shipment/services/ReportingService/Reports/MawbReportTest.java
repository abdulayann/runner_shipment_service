package com.dpw.runner.shipment.services.ReportingService.Reports;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ReportException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.service.impl.ConsolidationService;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class MawbReportTest extends CommonMocks {

    @InjectMocks
    private MawbReport mawbReport;

    @Mock
    private HawbReport hawbReport;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IAwbRepository awbRepository;

    @Mock
    private IAwbDao awbDao;

    @Mock
    private IAwbService awbService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private ConsolidationDao consolidationDao;

    @Mock
    private ShipmentService shipmentService;

    @Mock
    private ConsolidationService consolidationService;

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
    private ConsolidationDetails consolidationDetails;

    @BeforeEach
    void setup() {
        consolidationDetails = new ConsolidationDetails();
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
    void testValidatePrinting_ModuleValidationDisabled() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.FALSE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        Assertions.assertDoesNotThrow(() -> mawbReport.validatePrinting(123L));
        verifyNoInteractions(shipmentDao, consolidationDao, shipmentService, consolidationService);
    }

    @Test
    void testValidatePrinting_ConsolidationNull() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.TRUE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(null);
        mawbReport.isDMawb = false;
        Assertions.assertThrows(ReportException.class, () -> mawbReport.validatePrinting(123L));
    }

    @Test
    void testValidatePrinting_ShipmentNull() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.TRUE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        mawbReport.isDMawb = true;

        Assertions.assertThrows(ReportException.class, () -> mawbReport.validatePrinting(123L));
    }

    @Test
    void testValidatePrinting_ConsolidationValidation() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.TRUE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
        consolidationDetails.setContainerCategory(Constants.CARGO_TYPE_LSE);
        consolidationDetails.setConsolidationType(Constants.CONSOLIDATION_TYPE_DRT);
        mawbReport.isDMawb = false;

        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        doNothing().when(consolidationService).validateCarrierDetails(any(), anyList());
        doNothing().when(consolidationService).validateMawbDetails(any(), anyList());

        Assertions.assertDoesNotThrow(() -> mawbReport.validatePrinting(123L));
    }

    @Test
    void testValidatePrinting_ShipmentValidation() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.TRUE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_LSE);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        mawbReport.isDMawb = true;

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentService).validateCarrierDetails(any(), anyList());
        doNothing().when(shipmentService).validateMawbDetails(any(), anyList());

        Assertions.assertDoesNotThrow(() -> mawbReport.validatePrinting(123L));
    }

    @Test
    void testValidatePrinting_MissingFields() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.TRUE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_LSE);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        mawbReport.isDMawb = true;

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doAnswer(invocation -> {
            List<ModuleValidationFieldType> missingFields = invocation.getArgument(1);
            missingFields.add(ModuleValidationFieldType.CARRIER);
            return null;
        }).when(shipmentService).validateCarrierDetails(any(), anyList());

        Assertions.assertThrows(ReportException.class, () -> mawbReport.validatePrinting(123L));
    }

    @Test
    void getDocumentModel() {
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(awbDao.findByConsolidationId(any())).thenReturn(Arrays.asList(new Awb()));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationModel.setHazardous(true);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mawbReport.isDMawb = false;
        mockShipmentSettings();
        Assertions.assertNotNull(mawbReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_dgError() {
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationModel.setHazardous(true);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mawbReport.isDMawb = false;
        UserContext.getUser().setPermissions(new HashMap<>());
        mockShipmentSettings();
        Assertions.assertThrows(ValidationException.class, () -> mawbReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModelDMawb() {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        when(awbDao.findByConsolidationId(any())).thenReturn(null);
        mawbReport.isDMawb = true;
        mockShipmentSettings();
        Assertions.assertNotNull(mawbReport.getDocumentModel(123L));
    }

    @Test
    void populateDictionary() {
        HawbModel hawbModel = new HawbModel();
        Assertions.assertNotNull(mawbReport.populateDictionary(hawbModel));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity_MAWB() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails1);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationModel.setShipmentType(Constants.DIRECTION_EXP);
        when(modelMapper.map(consolidationDetails1, ConsolidationModel.class)).thenReturn(consolidationModel);
        mawbReport.isDMawb = false;
        mockShipmentSettings();

        Assertions.assertThrows(ValidationException.class, () -> mawbReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity_MAWB2() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails1);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationModel.setShipmentType(Constants.DIRECTION_EXP);
        when(modelMapper.map(consolidationDetails1, ConsolidationModel.class)).thenReturn(consolidationModel);
        mawbReport.isDMawb = false;
        mawbReport.printType = ReportConstants.ORIGINAL;
        mockShipmentSettings();

        Assertions.assertThrows(ValidationException.class, () -> mawbReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity_MAWB3() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails1);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationModel.setShipmentType(Constants.DIRECTION_EXP);
        consolidationModel.setHazardous(true);
        when(modelMapper.map(consolidationDetails1, ConsolidationModel.class)).thenReturn(consolidationModel);
        mawbReport.isDMawb = false;
        mawbReport.printType = ReportConstants.ORIGINAL;
        mockShipmentSettings();

        Assertions.assertThrows(ValidationException.class, () -> mawbReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity_DMAWB() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentModel.setDirection(Constants.DIRECTION_EXP);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        mawbReport.isDMawb = true;

        mockShipmentSettings();

        Assertions.assertThrows(ValidationException.class, () -> mawbReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity_DMAWB2() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentModel.setDirection(Constants.DIRECTION_EXP);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        mawbReport.isDMawb = true;
        mawbReport.printType = ReportConstants.ORIGINAL;

        mockShipmentSettings();

        Assertions.assertThrows(ValidationException.class, () -> mawbReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity_DMAWB3() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentModel.setDirection(Constants.DIRECTION_EXP);
        shipmentModel.setContainsHazardous(true);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        mawbReport.isDMawb = true;
        mawbReport.printType = ReportConstants.ORIGINAL;

        mockShipmentSettings();

        Assertions.assertThrows(ValidationException.class, () -> mawbReport.getDocumentModel(123L));
    }
}
