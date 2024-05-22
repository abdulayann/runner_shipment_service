package com.dpw.runner.shipment.services.entitytransfer.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.CheckTaskExistResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsolidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ValidationResponse;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
@ExtendWith(MockitoExtension.class)
class EntityTransferServiceTest {

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IShipmentService shipmentService;
    @Mock
    private IConsolidationService consolidationService;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private IV1Service v1Service;
    @Mock
    private TenantContext tenantContext;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IHblDao hblDao;
    @Mock
    private IAwbDao awbDao;
    @Mock
    private IEventDao eventDao;
    @Mock
    private V1MasterDataImpl v1MasterData;
    @Mock
    MasterDataFactory masterDataFactory;
    @InjectMocks
    private EntityTransferService entityTransferService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapperTest = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
//        testairMessagingLogs = jsonTestUtility.getTestAirMessagingLogs();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }


    @Test
    void testSendShipment_Success() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setReceivingBranch(66L);
        shipmentDetails.setTriangulationPartner(69L);
        shipmentDetails.setDocumentationPartner(66L);
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();
        SendShipmentRequest request = SendShipmentRequest.builder()
                .shipId(shipmentDetails.getId())
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(organizations.getOrganizationCode()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .build();
        SendEntityResponse v1ShipmentTaskResponse = new SendEntityResponse();
        v1ShipmentTaskResponse.setIsCreated(true);
        SendShipmentResponse sendShipmentResponse = SendShipmentResponse.builder().successTenantIds(List.of(66, 69)).build();
        List<V1TenantResponse> v1TenantResponseList = List.of(V1TenantResponse.builder().TenantName("Canada").build(), V1TenantResponse.builder().TenantName("HIPL").build());
        V1DataResponse v1DataResponseTenant = V1DataResponse.builder().entities(v1TenantResponseList).build();

        when(shipmentDao.findById(shipmentDetails.getId())).thenReturn(Optional.of(shipmentDetails));
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponseOrg);
        when(jsonHelper.convertValueToList(List.of(organizations), EntityTransferOrganizations.class)).thenReturn(List.of(organizations));
        when(v1Service.tenantByGuid(any())).thenReturn(TenantIdResponse.builder().id(69).build());
        when(v1Service.sendV1ShipmentTask(any())).thenReturn(v1ShipmentTaskResponse);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(v1DataResponseTenant);
        when(jsonHelper.convertValueToList(v1DataResponseTenant.entities, V1TenantResponse.class)).thenReturn(v1TenantResponseList);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendShipment(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(sendShipmentResponse), responseEntity);
    }

    @Test
    void testSendShipment_Failure() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        SendShipmentRequest request = SendShipmentRequest.builder()
                .shipId(shipmentDetails.getId())
                .sendToBranch(null)
                .sendToOrg(null)
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipment(commonRequestModel));
    }

    @Test
    void testSendShipment_Failure_DataRetrievalException() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        SendShipmentRequest request = SendShipmentRequest.builder()
                .shipId(shipmentDetails.getId())
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(organizations.getOrganizationCode()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(shipmentDao.findById(shipmentDetails.getId())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.sendShipment(commonRequestModel));
    }

    @Test
    void testSendShipment_Failure1() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();
        SendShipmentRequest request = SendShipmentRequest.builder()
                .shipId(shipmentDetails.getId())
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(organizations.getOrganizationCode()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(shipmentDetails.getId())).thenReturn(Optional.of(shipmentDetails));
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponseOrg);
        when(jsonHelper.convertValueToList(List.of(organizations), EntityTransferOrganizations.class)).thenReturn(List.of(organizations));
        when(v1Service.tenantByGuid(any())).thenReturn(TenantIdResponse.builder().id(69).build());
        when(v1Service.sendV1ShipmentTask(any())).thenThrow(new RuntimeException());
        assertThrows(RuntimeException.class, () -> entityTransferService.sendShipment(commonRequestModel));
    }

    @Test
    void testSendShipment_Failure2() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();
        SendShipmentRequest request = SendShipmentRequest.builder()
                .shipId(shipmentDetails.getId())
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(organizations.getOrganizationCode()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        SendEntityResponse v1ShipmentTaskResponse = new SendEntityResponse();
        v1ShipmentTaskResponse.setIsCreated(false);

        when(shipmentDao.findById(shipmentDetails.getId())).thenReturn(Optional.of(shipmentDetails));
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponseOrg);
        when(jsonHelper.convertValueToList(List.of(organizations), EntityTransferOrganizations.class)).thenReturn(List.of(organizations));
        when(v1Service.tenantByGuid(any())).thenReturn(TenantIdResponse.builder().id(69).build());
        when(v1Service.sendV1ShipmentTask(any())).thenReturn(v1ShipmentTaskResponse);
        assertThrows(RuntimeException.class, () -> entityTransferService.sendShipment(commonRequestModel));
    }

    @Test
    void testSendShipment_Failure3() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();
        SendShipmentRequest request = SendShipmentRequest.builder()
                .shipId(shipmentDetails.getId())
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(organizations.getOrganizationCode()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        organizations.setWhitelistedTenantGUID(null);

        when(shipmentDao.findById(shipmentDetails.getId())).thenReturn(Optional.of(shipmentDetails));
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponseOrg);
        when(jsonHelper.convertValueToList(List.of(organizations), EntityTransferOrganizations.class)).thenReturn(List.of(organizations));
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipment(commonRequestModel));
    }

    @Test
    void testSendConsolidation_Success() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        SendEntityResponse v1ConsoleTaskResponse = new SendEntityResponse();
        v1ConsoleTaskResponse.setIsCreated(true);
        List<V1TenantResponse> v1TenantResponseList = List.of(V1TenantResponse.builder().TenantName("Canada").build(), V1TenantResponse.builder().TenantName("HIPL").build());
        V1DataResponse v1DataResponseTenant = V1DataResponse.builder().entities(v1TenantResponseList).build();
        SendConsolidationResponse sendConsolidationResponse = SendConsolidationResponse.builder().successTenantIds(List.of(66, 69)).build();

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponseOrg);
        when(jsonHelper.convertValueToList(List.of(organizations), EntityTransferOrganizations.class)).thenReturn(List.of(organizations));
        when(v1Service.tenantByGuid(any())).thenReturn(TenantIdResponse.builder().id(69).build());
        when(v1Service.sendV1ConsolidationTask(any())).thenReturn(v1ConsoleTaskResponse);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(v1DataResponseTenant);
        when(jsonHelper.convertValueToList(v1DataResponseTenant.entities, V1TenantResponse.class)).thenReturn(v1TenantResponseList);

        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(sendConsolidationResponse), responseEntity);
    }

    @Test
    void testSendConsolidation_Failure() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(null)
                .consolId(consolidationDetails.getId())
                .sendToOrg(null)
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        assertThrows(ValidationException.class, ()-> entityTransferService.sendConsolidation(commonRequestModel));
    }

    @Test
    void testSendConsolidation_Failure_DataRetrievalError() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(null)
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);
        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, ()-> entityTransferService.sendConsolidation(commonRequestModel));
    }

    @Test
    void testSendConsolidation_Failure1() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponseOrg);
        when(jsonHelper.convertValueToList(List.of(organizations), EntityTransferOrganizations.class)).thenReturn(List.of(organizations));
        when(v1Service.tenantByGuid(any())).thenReturn(TenantIdResponse.builder().id(69).build());
        when(v1Service.sendV1ConsolidationTask(any())).thenThrow(new RuntimeException());

        assertThrows(RuntimeException.class, ()-> entityTransferService.sendConsolidation(commonRequestModel));
    }

    @Test
    void testSendConsolidation_Failure2() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        SendEntityResponse v1ConsoleTaskResponse = new SendEntityResponse();
        v1ConsoleTaskResponse.setIsCreated(false);

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponseOrg);
        when(jsonHelper.convertValueToList(List.of(organizations), EntityTransferOrganizations.class)).thenReturn(List.of(organizations));
        when(v1Service.tenantByGuid(any())).thenReturn(TenantIdResponse.builder().id(69).build());
        when(v1Service.sendV1ConsolidationTask(any())).thenReturn(v1ConsoleTaskResponse);

        assertThrows(RuntimeException.class, ()-> entityTransferService.sendConsolidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Success() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel = new TenantModel();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(hblDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of(new Hbl()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidationValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendConsolidationValidation_Failure_DataRetrievalException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_ConsoleFieldsException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setBol(null);
        consolidationDetails.getCarrierDetails().setVoyage(null);
        consolidationDetails.getCarrierDetails().setVessel(null);
        consolidationDetails.getCarrierDetails().setEta(null);
        consolidationDetails.getCarrierDetails().setEtd(null);
        consolidationDetails.getCarrierDetails().setOriginPort(null);
        consolidationDetails.getCarrierDetails().setDestinationPort(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_ShipmentFieldsException_Hbl() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.getCarrierDetails().setVessel(null);
        shipmentDetails.getCarrierDetails().setVoyage(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(hblDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }
    @Test
    void testSendConsolidationValidation_Failure_ShipmentFieldsException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.getCarrierDetails().setVessel(null);
        shipmentDetails.getCarrierDetails().setVoyage(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(hblDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of(new Hbl()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Success_Air() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails.getCarrierDetails().setShippingLine("Air India");
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber("A123");
        shipmentDetails.getCarrierDetails().setShippingLine("Air India");
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of(new Awb()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidationValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendConsolidationValidation_Failure_Air_ConsoleFieldsException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getCarrierDetails().setFlightNumber(null);
        consolidationDetails.setBol(null);
        consolidationDetails.getCarrierDetails().setShippingLine(null);
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber("A123");
        shipmentDetails.getCarrierDetails().setShippingLine("Air India");
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_Air_ShipmentFieldsException_Awb() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails.getCarrierDetails().setShippingLine("Air India");
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber(null);
        shipmentDetails.getCarrierDetails().setShippingLine(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of(new Awb()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_Air_ShipmentFieldsException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails.getCarrierDetails().setShippingLine("Air India");
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber(null);
        shipmentDetails.getCarrierDetails().setShippingLine(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Success() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        when(hblDao.findByShipmentId(shipmentDetails.getId())).thenReturn(List.of(new Hbl()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendShipmentValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendShipmentValidation_Failure_DataRetrievalFailure() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Failure_ShipmentFieldsException() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setMasterBill(null);
        shipmentDetails.getCarrierDetails().setVoyage(null);
        shipmentDetails.getCarrierDetails().setVessel(null);
        shipmentDetails.getCarrierDetails().setEta(null);
        shipmentDetails.getCarrierDetails().setEtd(null);
        shipmentDetails.getCarrierDetails().setOriginPort(null);
        shipmentDetails.getCarrierDetails().setDestinationPort(null);
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Failure_HblError() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        when(hblDao.findByShipmentId(shipmentDetails.getId())).thenReturn(List.of());
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Success_Air() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber("W233");
        shipmentDetails.getCarrierDetails().setShippingLine("Air India");
        shipmentDetails.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        when(awbDao.findByShipmentId(shipmentDetails.getId())).thenReturn(List.of(new Awb()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendShipmentValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendShipmentValidation_Failure_Air_ShipmentFieldsError() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber(null);
        shipmentDetails.getCarrierDetails().setShippingLine(null);
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setMasterBill(null);
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Failure_Air_AwbError() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber("W233");
        shipmentDetails.getCarrierDetails().setShippingLine("Air India");
        shipmentDetails.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        when(awbDao.findByShipmentId(shipmentDetails.getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testCheckTaskExist_Success_Shipment() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                .entityId(shipmentDetails.getId())
                .entityType(Constants.Shipments)
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(UUID.randomUUID().toString()))
                .build();
        CheckTaskExistResponse response = CheckTaskExistResponse.builder().sendToBranch(request.getSendToBranch()).sendToOrg(request.getSendToOrg()).build();

        when(shipmentDao.findById(request.getEntityId())).thenReturn(Optional.of(shipmentDetails));
        when(v1Service.checkTaskExist(any())).thenReturn(response);

        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.checkTaskExist(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testCheckTaskExist_Success_Shipment1() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                .entityId(shipmentDetails.getId())
                .entityType(Constants.Shipments)
                .sendToBranch(null)
                .sendToOrg(List.of(UUID.randomUUID().toString()))
                .build();
        CheckTaskExistResponse response = CheckTaskExistResponse.builder().sendToBranch(request.getSendToBranch()).sendToOrg(request.getSendToOrg()).build();

        when(shipmentDao.findById(request.getEntityId())).thenReturn(Optional.of(shipmentDetails));
        when(v1Service.checkTaskExist(any())).thenReturn(response);

        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.checkTaskExist(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testCheckTaskExist_Success_Shipment2() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                .entityId(shipmentDetails.getId())
                .entityType(Constants.Shipments)
                .sendToBranch(List.of(66))
                .sendToOrg(null)
                .build();
        CheckTaskExistResponse response = CheckTaskExistResponse.builder().sendToBranch(request.getSendToBranch()).sendToOrg(request.getSendToOrg()).build();

        when(shipmentDao.findById(request.getEntityId())).thenReturn(Optional.of(shipmentDetails));
        when(v1Service.checkTaskExist(any())).thenReturn(response);

        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.checkTaskExist(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testCheckTaskExist_Failure_Shipment() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                .entityId(shipmentDetails.getId())
                .entityType(Constants.Shipments)
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(UUID.randomUUID().toString()))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(request.getEntityId())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.checkTaskExist(commonRequestModel));
    }


    @Test
    void testCheckTaskExist_Success_Consolidation() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                .entityId(consolidationDetails.getId())
                .entityType(Constants.Consolidations)
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(UUID.randomUUID().toString()))
                .build();
        CheckTaskExistResponse response = CheckTaskExistResponse.builder().sendToBranch(request.getSendToBranch()).sendToOrg(request.getSendToOrg()).build();

        when(consolidationDetailsDao.findById(request.getEntityId())).thenReturn(Optional.of(consolidationDetails));
        when(v1Service.checkTaskExist(any())).thenReturn(response);

        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.checkTaskExist(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testCheckTaskExist_Failure_Consolidation() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                .entityId(consolidationDetails.getId())
                .entityType(Constants.Consolidations)
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(UUID.randomUUID().toString()))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(request.getEntityId())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.checkTaskExist(commonRequestModel));
    }

    @Test
    void testCheckTaskExist_Failure_Consolidation_V1Error() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                .entityId(consolidationDetails.getId())
                .entityType(Constants.Consolidations)
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(UUID.randomUUID().toString()))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(consolidationDetailsDao.findById(request.getEntityId())).thenReturn(Optional.of(consolidationDetails));
        when(v1Service.checkTaskExist(any())).thenThrow(new RuntimeException());

        assertThrows(RuntimeException.class, () -> entityTransferService.checkTaskExist(commonRequestModel));
    }
}