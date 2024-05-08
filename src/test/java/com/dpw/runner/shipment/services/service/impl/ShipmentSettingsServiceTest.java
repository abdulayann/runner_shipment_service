package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.DocumentService.DocumentService;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IHblTermsConditionTemplateDao;
import com.dpw.runner.shipment.services.dao.interfaces.IProductSequenceConfigDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITenantProductsDao;
import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.dto.request.TemplateUploadRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.TemplateUploadResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentSettingsSyncRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentSettingsServiceTest {

    @Mock
    IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    IHblTermsConditionTemplateDao hblTermsConditionTemplateDao;

    @Mock
    ITenantProductsDao tenantProductsDao;

    @Mock
    IProductSequenceConfigDao productSequenceConfigDao;

    @Mock
    IShipmentSettingsSync shipmentSettingsSync;

    @Mock
    JsonHelper jsonHelper;

    @Mock
    private DocumentService documentService;

    @InjectMocks
    private ShipmentSettingsService shipmentSettingsService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    private static ShipmentSettingsDetails testShipmentSettingsDetails;
    private static ShipmentSettingsDetails testShipmentSettingsDetails_New;
    private static ShipmentSettingRequest shipmentSettingRequest;
    private static ShipmentSettingsDetailsResponse shipmentSettingsDetailsResponse;
    private static ShipmentSettingsSyncRequest testShipmentSettingsSyncRequest;

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
        testShipmentSettingsDetails = jsonTestUtility.getTestShipmentSettingsDetails();
        testShipmentSettingsDetails_New = jsonTestUtility.getTestShipmentSettingsDetails_CreatePayload();
        shipmentSettingsDetailsResponse = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingsDetailsResponse.class);
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
    void create() {
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails_New, ShipmentSettingRequest.class);
        ShipmentSettingsService spyService = spy(shipmentSettingsService);
        doReturn(objectMapperTest.convertValue(testShipmentSettingsDetails_New, ShipmentSettingsDetails.class)).when(spyService).convertRequestToEntity(any());
        when(shipmentSettingsDao.save(any())).thenReturn(testShipmentSettingsDetails);
        when(hblTermsConditionTemplateDao.saveEntityFromSettings(any(), anyLong(), eq(true))).thenReturn(testShipmentSettingsDetails.getHblTermsConditionTemplate());
        when(hblTermsConditionTemplateDao.saveEntityFromSettings(any(), anyLong(), eq(false))).thenReturn(testShipmentSettingsDetails.getHblHawbBackPrintTemplate());
        when(tenantProductsDao.saveEntityFromSettings(any(), anyLong())).thenReturn(testShipmentSettingsDetails.getTenantProducts());
        when(tenantProductsDao.findAll(any(), any())).thenReturn(new PageImpl<>(testShipmentSettingsDetails.getTenantProducts()));
        when(productSequenceConfigDao.saveEntityFromSettings(any(), anyLong())).thenReturn(testShipmentSettingsDetails.getProductSequenceConfig());
        when(shipmentSettingsSync.sync(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetailsResponse.class))).thenReturn(objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingsDetailsResponse.class));
        ResponseEntity<IRunnerResponse> responseEntity = spyService.create(CommonRequestModel.buildRequest(shipmentSettingRequest));
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create_Failure() {
        ShipmentSettingsService spyService = spy(shipmentSettingsService);
        when(shipmentSettingsDao.save(any())).thenThrow(new RuntimeException());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        var e = Assertions.assertThrows(RuntimeException.class, () -> spyService.create(commonRequestModel));
        Assertions.assertNotNull(e);
    }

    @Test
    void update() {
        shipmentSettingsService.update(CommonRequestModel.buildRequest()); // test case not required since function is empty
        Assertions.assertTrue(true);
    }

    @Test
    void list() {
        shipmentSettingsService.list(CommonRequestModel.buildRequest()); // test case not required since function is empty
        Assertions.assertTrue(true);
    }

    @Test
    void listAsync() {
        shipmentSettingsService.listAsync(CommonRequestModel.buildRequest()); // test case not required since function is empty
        Assertions.assertTrue(true);
    }

    @Test
    void delete() {
        shipmentSettingsService.delete(CommonRequestModel.buildRequest()); // test case not required since function is empty
        Assertions.assertTrue(true);
    }

    @Test
    void completeUpdate() throws RunnerException {
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        ShipmentSettingsService spyService = spy(shipmentSettingsService);
        when(shipmentSettingsDao.list(any(), any())).thenReturn(new PageImpl<>(List.of(testShipmentSettingsDetails)));
        doReturn(objectMapperTest.convertValue(testShipmentSettingsDetails_New, ShipmentSettingsDetails.class)).when(spyService).convertRequestToEntity(any());
        when(shipmentSettingsDao.save(any())).thenReturn(testShipmentSettingsDetails);
        when(hblTermsConditionTemplateDao.updateEntityFromSettings(any(), anyLong(), eq(true))).thenReturn(testShipmentSettingsDetails.getHblTermsConditionTemplate());
        when(hblTermsConditionTemplateDao.updateEntityFromSettings(any(), anyLong(), eq(false))).thenReturn(testShipmentSettingsDetails.getHblHawbBackPrintTemplate());
        when(tenantProductsDao.updateEntityFromSettings(any(), anyLong())).thenReturn(testShipmentSettingsDetails.getTenantProducts());
        when(tenantProductsDao.findAll(any(), any())).thenReturn(new PageImpl<>(testShipmentSettingsDetails.getTenantProducts()));
        when(productSequenceConfigDao.updateEntityFromSettings(any(), anyLong())).thenReturn(testShipmentSettingsDetails.getProductSequenceConfig());
        when(shipmentSettingsSync.sync(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetailsResponse.class))).thenReturn(objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingsDetailsResponse.class));
        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeUpdate(CommonRequestModel.buildRequest(shipmentSettingRequest));
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void completeUpdate_failure() throws RunnerException {
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        ShipmentSettingsService spyService = spy(shipmentSettingsService);
        when(shipmentSettingsDao.list(any(), any())).thenReturn(new PageImpl<>(List.of(testShipmentSettingsDetails)));
        doReturn(objectMapperTest.convertValue(testShipmentSettingsDetails_New, ShipmentSettingsDetails.class)).when(spyService).convertRequestToEntity(any());
        when(shipmentSettingsDao.save(any())).thenThrow(new RuntimeException());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentSettingRequest);
        Assertions.assertThrows(RuntimeException.class, () -> spyService.completeUpdate(commonRequestModel));
    }

    @Test
    void completeUpdate_RequestNull() throws RunnerException{
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Assertions.assertThrows(RuntimeException.class, () -> shipmentSettingsService.completeUpdate(commonRequestModel));
    }

    @Test
    void completeUpdate_IdNull() throws RunnerException{
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        shipmentSettingRequest.setId(null);
        shipmentSettingRequest.setTenantId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentSettingRequest);
        Assertions.assertThrows(DataRetrievalFailureException.class, () -> shipmentSettingsService.completeUpdate(commonRequestModel));
    }

    @Test
    void completeUpdate_TenantIdNull() throws RunnerException{
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        shipmentSettingRequest.setTenantId(null);
        when(shipmentSettingsDao.findById(anyLong())).thenReturn(Optional.empty());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentSettingRequest);
        Assertions.assertThrows(DataRetrievalFailureException.class, () -> shipmentSettingsService.completeUpdate(commonRequestModel));
    }

    @Test
    void completeSettingsUpdateCreateV1_Create() throws RunnerException{
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        when(shipmentSettingsDao.list(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>()));
        when(shipmentSettingsDao.save(any())).thenReturn(testShipmentSettingsDetails);
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetailsResponse.class))).thenReturn(shipmentSettingsDetailsResponse);
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetails.class))).thenReturn(testShipmentSettingsDetails);
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.completeSettingsUpdateCreateV1(CommonRequestModel.buildRequest(shipmentSettingRequest));
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void completeSettingsUpdateCreateV1_Update() throws RunnerException{
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        when(shipmentSettingsDao.list(any(), any())).thenReturn(new PageImpl<>(List.of(testShipmentSettingsDetails)));
        when(shipmentSettingsDao.save(any())).thenReturn(testShipmentSettingsDetails);
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetailsResponse.class))).thenReturn(shipmentSettingsDetailsResponse);
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetails.class))).thenReturn(testShipmentSettingsDetails);
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.completeSettingsUpdateCreateV1(CommonRequestModel.buildRequest(shipmentSettingRequest));
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCompleteSettingsUpdateCreateV1_RequestNull() throws RunnerException{
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Assertions.assertThrows(RuntimeException.class, () -> shipmentSettingsService.completeSettingsUpdateCreateV1(commonRequestModel));
    }

    @Test
    void testCompleteSettingsUpdateCreateV1_RequestTenantIdNull() throws RunnerException{
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        shipmentSettingRequest.setTenantId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentSettingRequest);
        Assertions.assertThrows(RuntimeException.class, () -> shipmentSettingsService.completeSettingsUpdateCreateV1(commonRequestModel));
    }

    @Test
    void testCompleteSettingsUpdateCreateV1_FailCreate() throws RunnerException{
        ShipmentSettingsService spyService = spy(shipmentSettingsService);
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        when(shipmentSettingsDao.list(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>()));
        doThrow(new RunnerException("")).when(spyService).completeCreateFromV1(any());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentSettingRequest);
        Assertions.assertThrows(RuntimeException.class, () -> spyService.completeSettingsUpdateCreateV1(commonRequestModel));
    }

    @Test
    void testCompleteSettingsUpdateCreateV1_FailUpdate() throws RunnerException{
        ShipmentSettingsService spyService = spy(shipmentSettingsService);
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        when(shipmentSettingsDao.list(any(), any())).thenReturn(new PageImpl<>(List.of(testShipmentSettingsDetails)));
        doThrow(new RunnerException("")).when(spyService).completeUpdateFromV1(any(), any());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentSettingRequest);
        Assertions.assertThrows(RuntimeException.class, () -> spyService.completeSettingsUpdateCreateV1(commonRequestModel));
    }

    @Test
    void testCompleteCreateFromV1() throws RunnerException{
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        when(shipmentSettingsDao.save(any())).thenThrow(new RuntimeException());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentSettingRequest);
        Assertions.assertThrows(RuntimeException.class, () -> shipmentSettingsService.completeCreateFromV1(commonRequestModel));
    }

    @Test
    void testCompleteCreateFromV1_Branches() throws RunnerException{
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        shipmentSettingRequest.setHblTermsConditionTemplate(new ArrayList<>());
        shipmentSettingRequest.setHblHawbBackPrintTemplate(new ArrayList<>());
        shipmentSettingRequest.setTenantProducts(new ArrayList<>());
        shipmentSettingRequest.setProductSequenceConfig(new ArrayList<>());
        when(shipmentSettingsDao.save(any())).thenReturn(testShipmentSettingsDetails);
        ResponseEntity<IRunnerResponse> responseEntity  = shipmentSettingsService.completeCreateFromV1(CommonRequestModel.buildRequest(shipmentSettingRequest));
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCompleteCreateFromV1_RequestNull() throws RunnerException{
        when(shipmentSettingsDao.save(any())).thenThrow(new RuntimeException());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Assertions.assertThrows(RuntimeException.class, () -> shipmentSettingsService.completeCreateFromV1(commonRequestModel));
    }

    @Test
    void testCompleteUpdateFromV1() throws RunnerException{
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        when(shipmentSettingsDao.save(any())).thenThrow(new RuntimeException());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentSettingRequest);
        Optional<ShipmentSettingsDetails> shipmentSettingsDetails = Optional.of(testShipmentSettingsDetails);
        Assertions.assertThrows(RuntimeException.class, () -> shipmentSettingsService.completeUpdateFromV1(shipmentSettingsDetails, commonRequestModel));
    }

    @Test
    void uploadTemplate_Create() {
        TemplateUploadRequest templateUploadRequest = new TemplateUploadRequest();
        ResponseEntity<TemplateUploadResponse> docServiceResponse = new ResponseEntity<>(HttpStatus.CREATED);
        when(documentService.createDocumentTemplate(any())).thenReturn(docServiceResponse);
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.uploadTemplate(CommonRequestModel.buildRequest(templateUploadRequest));
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void uploadTemplate_Create_LengthZero() {
        TemplateUploadRequest templateUploadRequest = new TemplateUploadRequest();
        templateUploadRequest.setPreviousFileId("");
        ResponseEntity<TemplateUploadResponse> docServiceResponse = new ResponseEntity<>(HttpStatus.CREATED);
        when(documentService.createDocumentTemplate(any())).thenReturn(docServiceResponse);
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.uploadTemplate(CommonRequestModel.buildRequest(templateUploadRequest));
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void uploadTemplate_CreateFailure() {
        TemplateUploadRequest templateUploadRequest = new TemplateUploadRequest();
        ResponseEntity<TemplateUploadResponse> docServiceResponse = new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        when(documentService.createDocumentTemplate(any())).thenReturn(docServiceResponse);
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.uploadTemplate(CommonRequestModel.buildRequest(templateUploadRequest));
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void uploadTemplate_CreateFailureError() {
        TemplateUploadRequest templateUploadRequest = new TemplateUploadRequest();
        ResponseEntity<TemplateUploadResponse> docServiceResponse = new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        when(documentService.createDocumentTemplate(any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.uploadTemplate(CommonRequestModel.buildRequest(templateUploadRequest));
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void uploadTemplate_Update() {
        TemplateUploadRequest templateUploadRequest = new TemplateUploadRequest();
        templateUploadRequest.setPreviousFileId("prv field");
        ResponseEntity<String> docServiceResponse = new ResponseEntity<>(HttpStatus.OK);
        when(documentService.updateDocumentTemplate(any())).thenReturn(docServiceResponse);
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.uploadTemplate(CommonRequestModel.buildRequest(templateUploadRequest));
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void uploadTemplate_UpdateFailure() {
        TemplateUploadRequest templateUploadRequest = new TemplateUploadRequest();
        templateUploadRequest.setPreviousFileId("prv field");
        ResponseEntity<String> docServiceResponse = new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        when(documentService.updateDocumentTemplate(any())).thenReturn(docServiceResponse);
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.uploadTemplate(CommonRequestModel.buildRequest(templateUploadRequest));
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void uploadTemplate_UpdateFailureError() {
        TemplateUploadRequest templateUploadRequest = new TemplateUploadRequest();
        templateUploadRequest.setPreviousFileId("prv field");
        ResponseEntity<String> docServiceResponse = new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        when(documentService.updateDocumentTemplate(any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.uploadTemplate(CommonRequestModel.buildRequest(templateUploadRequest));
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void downloadTemplate() throws RunnerException{
        byte[] response = new byte[0];
        when(documentService.downloadTemplate(any())).thenReturn(response);
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.downloadTemplate("tempId");
        Assertions.assertNotNull(responseEntity);
    }

    @Test
    void downloadTemplate_Failure() throws RunnerException{
        byte[] response = new byte[0];
        when(documentService.downloadTemplate(any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.downloadTemplate("tempId");
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByTenantId() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(testShipmentSettingsDetails));
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveByTenantId(CommonRequestModel.buildRequest(commonGetRequest));
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByTenantId_IdNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveByTenantId(CommonRequestModel.buildRequest(commonGetRequest));
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByTenantId_RequestNull() {
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveByTenantId(CommonRequestModel.buildRequest());
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByTenantId_SettingsNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveByTenantId(CommonRequestModel.buildRequest(commonGetRequest));
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByTenantId_Error() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        when(shipmentSettingsDao.findByTenantId(any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveByTenantId(CommonRequestModel.buildRequest(commonGetRequest));
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).guid(UUID.randomUUID().toString()).build();
        when(shipmentSettingsDao.findById(anyLong())).thenReturn(Optional.of(testShipmentSettingsDetails));
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveById(CommonRequestModel.buildRequest(commonGetRequest));
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_requestNull() {
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveById(CommonRequestModel.buildRequest((com.dpw.runner.shipment.services.commons.requests.IRunnerRequest) null));
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_IdNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveById(CommonRequestModel.buildRequest(commonGetRequest));
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_GuidNotNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build();
        when(shipmentSettingsDao.findByGuid(any())).thenReturn(Optional.of(testShipmentSettingsDetails));
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveById(CommonRequestModel.buildRequest(commonGetRequest));
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_GuidNotNull_Failure() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build();
        when(shipmentSettingsDao.findByGuid(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveById(CommonRequestModel.buildRequest(commonGetRequest));
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_Failure() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        when(shipmentSettingsDao.findById(anyLong())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentSettingsService.retrieveById(CommonRequestModel.buildRequest(commonGetRequest));
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

}
