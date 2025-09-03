package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.impl.PartiesDao;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.v3.PartiesValidationUtil;
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
class PartiesV3ServiceTest {

    @InjectMocks
    private PartiesV3Service partiesV3Service;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private PartiesDao partiesDao;
    @Mock
    private PartiesValidationUtil partiesValidationUtil;
    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static Parties testParties;
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
        testParties = jsonTestUtility.getParties();
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
        PartiesRequest request = new PartiesRequest();
        Parties partyEntity = new Parties();
        Parties savedEntity = new Parties();
        savedEntity.setId(123L); // set ID after "saving"
        PartiesResponse expectedResponse = new PartiesResponse();
        expectedResponse.setId(123L);
        when(jsonHelper.convertValue(request, Parties.class)).thenReturn(partyEntity);
        when(partiesDao.save(partyEntity)).thenReturn(savedEntity);
        when(jsonHelper.convertValue(savedEntity, PartiesResponse.class)).thenReturn(expectedResponse);
        // Act
        PartiesResponse actual = partiesV3Service.create(request);
        // Assert
        assertNotNull(actual);
        assertEquals(expectedResponse.getId(), actual.getId());
        verify(jsonHelper).convertValue(request, Parties.class);
        verify(partiesDao).save(partyEntity);
        verify(jsonHelper).convertValue(savedEntity, PartiesResponse.class);
    }
    @Test
    void testUpdate_success() {
        // Arrange
        PartiesRequest request = new PartiesRequest();
        request.setId(101L);
        Parties oldEntity = new Parties();
        oldEntity.setId(101L);
        Parties newEntity = new Parties();
        newEntity.setId(101L);
        Parties updatedEntity = new Parties();
        updatedEntity.setId(101L);
        PartiesResponse expectedResponse = new PartiesResponse();
        expectedResponse.setId(101L);
        // Mock behavior
        doNothing().when(partiesValidationUtil).validateUpdateRequest(request);
        when(partiesDao.findById(101L)).thenReturn(Optional.of(oldEntity));
        when(jsonHelper.convertValue(request, Parties.class)).thenReturn(newEntity);
        when(partiesDao.save(newEntity)).thenReturn(updatedEntity);
        when(jsonHelper.convertValue(updatedEntity, PartiesResponse.class)).thenReturn(expectedResponse);
        // Spy to verify audit
        PartiesV3Service spyService = Mockito.spy(partiesV3Service);
        // Act
        PartiesResponse actual = spyService.update(request);
        // Assert
        assertNotNull(actual);
        assertEquals(101L, actual.getId());
        verify(partiesValidationUtil).validateUpdateRequest(request);
        verify(partiesDao).findById(101L);
        verify(partiesDao).save(newEntity);
    }
    @Test
    void testUpdate_entityNotFound_throwsException() {
        // Arrange
        PartiesRequest request = new PartiesRequest();
        request.setId(404L);
        doNothing().when(partiesValidationUtil).validateUpdateRequest(request);
        when(partiesDao.findById(404L)).thenReturn(Optional.empty());
        // Act & Assert
        assertThrows(DataRetrievalFailureException.class, () -> partiesV3Service.update(request));
        verify(partiesValidationUtil).validateUpdateRequest(request);
        verify(partiesDao).findById(404L);
    }
    @Test
    void testList_success33() {
        List<Parties> parties = Collections.singletonList(testParties);
        PartiesResponse response = mock(PartiesResponse.class);
        var listRequest = new ListCommonRequest();
        PartiesV3Service spyService = spy(partiesV3Service);
        doReturn(new PageImpl<>(parties)).when(partiesDao).findAll(any(), any());
        when(jsonHelper.convertValue(any(), eq(PartiesResponse.class))).thenReturn(response);
        List<PartiesResponse> partiesResponses = spyService.list(listRequest);
        assertNotNull(partiesResponses);
    }
    @Test
    void testDelete_success() {
        // Arrange
        Long id = 1L;
        PartiesRequest request = new PartiesRequest();
        request.setId(id);
        Parties parties = new Parties();
        parties.setId(id);
        PartiesValidationUtil validationUtil = mock(PartiesValidationUtil.class);
        PartiesV3Service service = spy(new PartiesV3Service());
        ReflectionTestUtils.setField(service, "partiesDao", this.partiesDao);
        ReflectionTestUtils.setField(service, "partiesValidationUtil", validationUtil);
        doNothing().when(validationUtil).validateUpdateRequest(request);
        when(partiesDao.findById(id)).thenReturn(Optional.of(parties));
        doNothing().when(partiesDao).delete(parties);
        // Act
        PartiesResponse result = service.delete(request);
        // Assert
        assertNotNull(result);
        assertEquals("Party deleted successfully!", result.getMessage());
        // Verify interactions
        verify(validationUtil).validateUpdateRequest(request);
        verify(partiesDao).findById(id);
        verify(partiesDao).delete(parties);
    }
    @Test
    void testDelete_entityNotFound_throwsException() {
        // Arrange
        PartiesRequest request = new PartiesRequest();
        request.setId(404L);
        doNothing().when(partiesValidationUtil).validateUpdateRequest(request);
        when(partiesDao.findById(404L)).thenReturn(Optional.empty());
        // Act & Assert
        assertThrows(DataRetrievalFailureException.class, () -> partiesV3Service.delete(request));
        verify(partiesValidationUtil).validateUpdateRequest(request);
        verify(partiesDao).findById(404L);
    }
}
