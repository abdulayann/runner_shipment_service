package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.RAKCDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PickupDeliveryDetailsServiceTest {

    @Mock
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private AuditLogService auditLogService;

    @Mock
    private KafkaProducer producer;

    @Mock
    private IV1Service v1Service;

    @InjectMocks
    private PickupDeliveryDetailsService pickupDeliveryDetailsService;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        UserContext.setUser(UsersDto.builder().TenantId(1).Username("test").build());
    }

    @Test
    void testCreate_Success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setShipmentId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.save(any(PickupDeliveryDetails.class))).thenReturn(entity);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetails.class))).thenReturn(entity);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(new PickupDeliveryDetailsResponse());
        when(pickupDeliveryDetailsDao.findByShipmentId(anyLong())).thenReturn(List.of(entity));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        doNothing().when(producer).produceToKafka(any(), anyString(), anyString());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.create(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).save(any(PickupDeliveryDetails.class));
        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
    void testCreate_failed_nullRequest() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        PickupDeliveryDetailsRequest request = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.create(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testCreate_Exception() {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetails.class))).thenReturn(new PickupDeliveryDetails());
        when(pickupDeliveryDetailsDao.save(any(PickupDeliveryDetails.class))).thenThrow(new RuntimeException("Error"));

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.create(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).save(any(PickupDeliveryDetails.class));
    }

    @Test
    void testUpdate_Success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        request.setId(1L);
        PickupDeliveryDetails oldEntity = new PickupDeliveryDetails();
        PickupDeliveryDetails newEntity = new PickupDeliveryDetails();
        newEntity.setId(1L);
        newEntity.setShipmentId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(oldEntity));
        when(pickupDeliveryDetailsDao.save(any(PickupDeliveryDetails.class))).thenReturn(newEntity);
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(jsonHelper.readFromJson(anyString(), eq(PickupDeliveryDetails.class))).thenReturn(oldEntity);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetails.class))).thenReturn(newEntity);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(new PickupDeliveryDetailsResponse());
        when(pickupDeliveryDetailsDao.findByShipmentId(anyLong())).thenReturn(List.of(newEntity));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        doNothing().when(producer).produceToKafka(any(), anyString(), anyString());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.update(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
        verify(pickupDeliveryDetailsDao, times(1)).save(any(PickupDeliveryDetails.class));
        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
    void testUpdate_Success_null_request() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        PickupDeliveryDetailsRequest request = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.update(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }


    @Test
    void testUpdate_Success_null_request_id() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        request.setId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.update(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testUpdate_Exception() throws RunnerException {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        request.setId(1L);
        PickupDeliveryDetails newEntity = new PickupDeliveryDetails();
        newEntity.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
//        when(jsonHelper.readFromJson(anyString(), eq(PickupDeliveryDetails.class))).thenReturn(oldEntity);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetails.class))).thenReturn(newEntity);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(new PickupDeliveryDetails()));
        when(pickupDeliveryDetailsDao.save(any(PickupDeliveryDetails.class))).thenThrow(new RuntimeException("Error"));

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.update(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
        verify(pickupDeliveryDetailsDao, times(1)).save(any(PickupDeliveryDetails.class));
    }

    @Test
    void testList_Success() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        Page<PickupDeliveryDetails> page = new PageImpl<>(Collections.singletonList(entity), PageRequest.of(0, 10), 1);

        when(pickupDeliveryDetailsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(new PickupDeliveryDetailsResponse());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testList_Success1() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        request.setPopulateRAKC(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setTransporterDetail(Parties.builder().orgId("1L").build());
        entity.setSourceDetail(Parties.builder().addressId("1L").build());
        entity.setDestinationDetail(Parties.builder().build());
        Page<PickupDeliveryDetails> page = new PageImpl<>(Collections.singletonList(entity), PageRequest.of(0, 10), 1);

        when(pickupDeliveryDetailsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(new PickupDeliveryDetailsResponse());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }


    @Test
    void testList_Success2() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        request.setPopulateRAKC(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Parties parties = Parties.builder().orgId("1L").addressId("10").build();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setTransporterDetail(parties);
        entity.setSourceDetail(parties);
        entity.setDestinationDetail(parties);
        Page<PickupDeliveryDetails> page = new PageImpl<>(Collections.singletonList(entity), PageRequest.of(0, 10), 1);
        List<RAKCDetailsResponse> rakcDetailsResponses = new ArrayList<>();
        rakcDetailsResponses.add(RAKCDetailsResponse.builder().id(10L).build());
        PartiesResponse partiesResponse = PartiesResponse.builder().orgId("1L").addressId("10").build();
        PickupDeliveryDetailsResponse pickupDeliveryDetailsResponse = new PickupDeliveryDetailsResponse();
        pickupDeliveryDetailsResponse.setId(1L);
        pickupDeliveryDetailsResponse.setTransporterDetail(partiesResponse);
        pickupDeliveryDetailsResponse.setSourceDetail(partiesResponse);
        pickupDeliveryDetailsResponse.setDestinationDetail(partiesResponse);


        when(pickupDeliveryDetailsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(pickupDeliveryDetailsResponse);
        when(v1Service.addressList(any())).thenReturn(V1DataResponse.builder().entities(new Object()).build());
        when(jsonHelper.convertValueToList(any(), eq(RAKCDetailsResponse.class))).thenReturn(rakcDetailsResponses);

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testList_Success3() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        request.setPopulateRAKC(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Parties parties = Parties.builder().orgId("1L").addressId("10").build();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setTransporterDetail(parties);
        entity.setSourceDetail(parties);
        entity.setIsDirectDelivery(true);
        Page<PickupDeliveryDetails> page = new PageImpl<>(Collections.singletonList(entity), PageRequest.of(0, 10), 1);
        List<RAKCDetailsResponse> rakcDetailsResponses = new ArrayList<>();
        rakcDetailsResponses.add(RAKCDetailsResponse.builder().id(10L).build());
        PartiesResponse partiesResponse = PartiesResponse.builder().orgId("1L").addressId("10").build();
        PickupDeliveryDetailsResponse pickupDeliveryDetailsResponse = new PickupDeliveryDetailsResponse();
        pickupDeliveryDetailsResponse.setId(1L);
        pickupDeliveryDetailsResponse.setTransporterDetail(partiesResponse);
        pickupDeliveryDetailsResponse.setSourceDetail(partiesResponse);
        pickupDeliveryDetailsResponse.setIsDirectDelivery(true);

        when(pickupDeliveryDetailsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(pickupDeliveryDetailsResponse);
        when(v1Service.addressList(any())).thenReturn(V1DataResponse.builder().entities(new Object()).build());
        when(jsonHelper.convertValueToList(any(), eq(RAKCDetailsResponse.class))).thenReturn(rakcDetailsResponses);

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testList_Success4() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        request.setPopulateRAKC(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Parties parties = Parties.builder().orgId("1L").addressId("10").build();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setTransporterDetail(parties);
        entity.setSourceDetail(parties);
        Page<PickupDeliveryDetails> page = new PageImpl<>(Collections.singletonList(entity), PageRequest.of(0, 10), 1);
        List<RAKCDetailsResponse> rakcDetailsResponses = new ArrayList<>();
        rakcDetailsResponses.add(RAKCDetailsResponse.builder().id(10L).build());
        PickupDeliveryDetailsResponse pickupDeliveryDetailsResponse = new PickupDeliveryDetailsResponse();
        pickupDeliveryDetailsResponse.setId(1L);
        pickupDeliveryDetailsResponse.setTransporterDetail(PartiesResponse.builder().orgId("1L").build());
        pickupDeliveryDetailsResponse.setSourceDetail(PartiesResponse.builder().addressId("10").build());

        when(pickupDeliveryDetailsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(pickupDeliveryDetailsResponse);
        when(v1Service.addressList(any())).thenReturn(V1DataResponse.builder().entities(new Object()).build());
        when(jsonHelper.convertValueToList(any(), eq(RAKCDetailsResponse.class))).thenReturn(rakcDetailsResponses);

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testList_Exception() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);

        when(pickupDeliveryDetailsDao.findAll(any(Specification.class), any(Pageable.class))).thenThrow(new RuntimeException("Error"));

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testDelete_Success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setShipmentId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(entity));
        when(pickupDeliveryDetailsDao.findByShipmentId(anyLong())).thenReturn(new ArrayList<>());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.delete(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
        verify(pickupDeliveryDetailsDao, times(1)).delete(any(PickupDeliveryDetails.class));
        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
    void testDelete_Exception() {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(new PickupDeliveryDetails()));
        doThrow(new RuntimeException("Error")).when(pickupDeliveryDetailsDao).delete(any(PickupDeliveryDetails.class));

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.delete(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
        verify(pickupDeliveryDetailsDao, times(1)).delete(any(PickupDeliveryDetails.class));
    }

    @Test
    void testRetrieveById_Success() {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(entity));
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(new PickupDeliveryDetailsResponse());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.retrieveById(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
    }

    @Test
    void testRetrieveById_Exception() {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.retrieveById(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());


        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
    }

    @Test
    void testRetrieveById_NullRequest() {
        CommonGetRequest request = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.retrieveById(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testRetrieveById_NullId() {
        CommonGetRequest request = CommonGetRequest.builder().id(null).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.retrieveById(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testDelete_NullId() {
        CommonGetRequest request = CommonGetRequest.builder().id(null).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.delete(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testDelete_NullRequest() {
        CommonGetRequest request = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.delete(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }
}
