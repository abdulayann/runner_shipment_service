package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.impl.ReferenceNumbersDao;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.v3.ReferenceNumbersValidationUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ReferenceNumbersV3ServiceTest {


    @InjectMocks
    private ReferenceNumbersV3Service referenceNumbersV3Service;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ReferenceNumbersDao referenceNumbersDao;

    @Mock
    private ReferenceNumbersValidationUtil referenceNumbersValidationUtil;


    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static ReferenceNumbers testReferenceNumbers;

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
        testReferenceNumbers = jsonTestUtility.getTestReferenceNumbers();
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
    void testCreate_success() {
        // Arrange
        ReferenceNumbersRequest request = new ReferenceNumbersRequest();
        ReferenceNumbers referenceEntity = new ReferenceNumbers();
        ReferenceNumbers savedEntity = new ReferenceNumbers();
        savedEntity.setId(123L); // set ID after "saving"

        ReferenceNumbersResponse expectedResponse = new ReferenceNumbersResponse();
        expectedResponse.setId(123L);

        when(jsonHelper.convertValue(request, ReferenceNumbers.class)).thenReturn(referenceEntity);
        when(referenceNumbersDao.save(referenceEntity)).thenReturn(savedEntity);
        when(jsonHelper.convertValue(savedEntity, ReferenceNumbersResponse.class)).thenReturn(expectedResponse);

        // Act
        ReferenceNumbersResponse actual = referenceNumbersV3Service.create(request);

        // Assert
        assertNotNull(actual);
        assertEquals(expectedResponse.getId(), actual.getId());

        verify(jsonHelper).convertValue(request, ReferenceNumbers.class);
        verify(referenceNumbersDao).save(referenceEntity);
        verify(jsonHelper).convertValue(savedEntity, ReferenceNumbersResponse.class);
    }


    @Test
    void testUpdate_success() {
        // Arrange
        ReferenceNumbersRequest request = new ReferenceNumbersRequest();
        request.setId(101L);

        ReferenceNumbers oldEntity = new ReferenceNumbers();
        oldEntity.setId(101L);

        ReferenceNumbers newEntity = new ReferenceNumbers();
        newEntity.setId(101L);

        ReferenceNumbers updatedEntity = new ReferenceNumbers();
        updatedEntity.setId(101L);

        ReferenceNumbersResponse expectedResponse = new ReferenceNumbersResponse();
        expectedResponse.setId(101L);

        // Mock behavior
        doNothing().when(referenceNumbersValidationUtil).validateUpdateRequest(request);
        when(referenceNumbersDao.findById(101L)).thenReturn(Optional.of(oldEntity));
        when(jsonHelper.convertValue(request, ReferenceNumbers.class)).thenReturn(newEntity);
        when(referenceNumbersDao.save(newEntity)).thenReturn(updatedEntity);
        when(jsonHelper.convertValue(updatedEntity, ReferenceNumbersResponse.class)).thenReturn(expectedResponse);

        // Spy to verify audit
        ReferenceNumbersV3Service spyService = Mockito.spy(referenceNumbersV3Service);
        //doNothing().when(spyService).recordAuditLogs(List.of(oldEntity), List.of(updatedEntity), DBOperationType.UPDATE);

        // Act
        ReferenceNumbersResponse actual = spyService.update(request);

        // Assert
        assertNotNull(actual);
        assertEquals(101L, actual.getId());

        verify(referenceNumbersValidationUtil).validateUpdateRequest(request);
        verify(referenceNumbersDao).findById(101L);
        verify(referenceNumbersDao).save(newEntity);
       // verify(spyService).recordAuditLogs(List.of(oldEntity), List.of(updatedEntity), DBOperationType.UPDATE);
    }


    @Test
    void testUpdate_entityNotFound_throwsException() {
        // Arrange
        ReferenceNumbersRequest request = new ReferenceNumbersRequest();
        request.setId(404L);

        doNothing().when(referenceNumbersValidationUtil).validateUpdateRequest(request);
        when(referenceNumbersDao.findById(404L)).thenReturn(Optional.empty());

        // Act & Assert
        assertThrows(DataRetrievalFailureException.class, () -> referenceNumbersV3Service.update(request));

        verify(referenceNumbersValidationUtil).validateUpdateRequest(request);
        verify(referenceNumbersDao).findById(404L);
    }

    @Test
    void testList_success33() {
        List<ReferenceNumbers> referenceNumbers = Collections.singletonList(testReferenceNumbers);
        ReferenceNumbersResponse response = mock(ReferenceNumbersResponse.class);
        var listRequest = new ListCommonRequest();
        ReferenceNumbersV3Service spyService = spy(referenceNumbersV3Service);
        doReturn(new PageImpl<>(referenceNumbers)).when(referenceNumbersDao).findAll(any(), any());
        when(jsonHelper.convertValue(any(), eq(ReferenceNumbersResponse.class))).thenReturn(response);
        List<ReferenceNumbersResponse> referenceNumbers1 = spyService.list(listRequest);
        assertNotNull(referenceNumbers1);
    }

    @Test
    void testDelete_success() {
        // Arrange
        Long id = 1L;

        ReferenceNumbersRequest request = new ReferenceNumbersRequest();
        request.setId(id);

        ReferenceNumbers referenceNumber = new ReferenceNumbers();
        referenceNumber.setId(id);

        ReferenceNumbersDao referenceNumbersDao = mock(ReferenceNumbersDao.class);
        ReferenceNumbersValidationUtil validationUtil = mock(ReferenceNumbersValidationUtil.class);
        ReferenceNumbersV3Service service = spy(new ReferenceNumbersV3Service());

        ReflectionTestUtils.setField(service, "referenceNumbersDao", referenceNumbersDao);
        ReflectionTestUtils.setField(service, "referenceNumbersValidationUtil", validationUtil);

        doNothing().when(validationUtil).validateUpdateRequest(request);
        when(referenceNumbersDao.findById(id)).thenReturn(Optional.of(referenceNumber));
        doNothing().when(referenceNumbersDao).delete(referenceNumber);
        //doNothing().when(service).recordAuditLogs(List.of(referenceNumber), null, DBOperationType.DELETE);

        // Act
        ReferenceNumbersResponse result = service.delete(request);

        // Assert
        assertNotNull(result);
        assertEquals("Reference number deleted successfully!", result.getMessage());

        // Verify interactions
        verify(validationUtil).validateUpdateRequest(request);
        verify(referenceNumbersDao).findById(id);
        verify(referenceNumbersDao).delete(referenceNumber);
        //verify(service).recordAuditLogs(List.of(referenceNumber), null, DBOperationType.DELETE);
    }

    @Test
    void testDelete_entityNotFound_throwsException() {
        // Arrange
        ReferenceNumbersRequest request = new ReferenceNumbersRequest();
        request.setId(404L);

        doNothing().when(referenceNumbersValidationUtil).validateUpdateRequest(request);
        when(referenceNumbersDao.findById(404L)).thenReturn(Optional.empty());

        // Act & Assert
        assertThrows(DataRetrievalFailureException.class, () -> referenceNumbersV3Service.delete(request));

        verify(referenceNumbersValidationUtil).validateUpdateRequest(request);
        verify(referenceNumbersDao).findById(404L);
    }

}
