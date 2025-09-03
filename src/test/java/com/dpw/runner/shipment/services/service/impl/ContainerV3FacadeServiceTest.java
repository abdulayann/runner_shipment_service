package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.ContainerDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ContainerV3PatchBulkUpdateRequest;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.mapper.ContainerV3Mapper;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ContainerV3FacadeServiceTest {
    @Mock
    private ContainerV3Service containerV3Service;

    @Mock
    private ContainerDao containerDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ContainerV3Mapper containerV3Mapper;

    @InjectMocks
    private ContainerV3FacadeService containerV3FacadeService; // Replace with your actual class name

    private static JsonTestUtility jsonTestUtility;

    private static ObjectMapper objectMapper;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapper = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    void testValidContainerNumber_AppendsLastDigit() {
        // Arrange
        ContainerV3Request request = new ContainerV3Request();
        request.setContainerNumber("ABC123");

        ContainerNumberCheckResponse response = new ContainerNumberCheckResponse();
        response.setSuccess(true);
        response.setLastDigit(4);

        when(containerV3Service.validateContainerNumber("ABC123")).thenReturn(response);

        List<ContainerV3Request> requestList = new ArrayList<>();
        requestList.add(request);

        // Act
        containerV3FacadeService.validateContainerNumberFormat(requestList);

        // Assert
        assertEquals("ABC1234", request.getContainerNumber());
        verify(containerV3Service, times(1)).validateContainerNumber("ABC123");
    }

    @Test
    void testInvalidContainerNumber_ThrowsException() {
        // Arrange
        ContainerV3Request request = new ContainerV3Request();
        request.setContainerNumber("INVALID");

        ContainerNumberCheckResponse response = new ContainerNumberCheckResponse();
        response.setSuccess(false);

        when(containerV3Service.validateContainerNumber("INVALID")).thenReturn(response);

        List<ContainerV3Request> requestList = Arrays.asList(request);

        // Act & Assert
        ValidationException ex = assertThrows(ValidationException.class,
                () -> containerV3FacadeService.validateContainerNumberFormat(requestList));
        assertEquals("Invalid container number format", ex.getMessage());

        verify(containerV3Service, times(1)).validateContainerNumber("INVALID");
    }

    @Test
    void testEmptyOrNullList_NoInteractionWithService() {
        // Null case
        containerV3FacadeService.validateContainerNumberFormat(null);
        verifyNoInteractions(containerV3Service);

        // Empty list case
        containerV3FacadeService.validateContainerNumberFormat(new ArrayList<>());
        verifyNoInteractions(containerV3Service);
    }

    @Test
    void testContainerNumberEmpty_SkipsValidation() {
        ContainerV3Request request = new ContainerV3Request();
        request.setContainerNumber(""); // Empty string

        List<ContainerV3Request> requestList = List.of(request);

        containerV3FacadeService.validateContainerNumberFormat(requestList);

        verifyNoInteractions(containerV3Service);
    }

    @Test
    void testUpdatePatchContainer() throws RunnerException {
        ContainerV3PatchBulkUpdateRequest request = new ContainerV3PatchBulkUpdateRequest();
        request.setContainerIds(List.of(1L, 2L));
        Containers containers = new Containers();
        containers.setConsolidationId(1L);
        when(containerDao.findByIdIn(anyList())).thenReturn(new ArrayList<>(List.of(containers)));
        ContainerV3Request containerV3Request = objectMapper.convertValue(containers, ContainerV3Request.class);
        ContainerResponse containerResponse = objectMapper.convertValue(containers, ContainerResponse.class);
        when(jsonHelper.convertValueToList(any(), eq(ContainerV3Request.class))).thenReturn(new ArrayList<>(List.of(containerV3Request)));
        when(containerV3Service.updateBulk(any(), any())).thenReturn(BulkContainerResponse.builder().containerResponseList(new ArrayList<>(List.of(containerResponse))).build());
        BulkContainerResponse response = containerV3FacadeService.updatePatchContainer(request, Constants.CONSOLIDATION);
        assertNotNull(response);
        assertEquals(1, response.getContainerResponseList().size());
    }

    @Test
    void testUpdatePatchContainer_NoContainerIds() throws RunnerException {
        ContainerV3PatchBulkUpdateRequest request = new ContainerV3PatchBulkUpdateRequest();
        assertThrows(ValidationException.class, () -> containerV3FacadeService.updatePatchContainer(request, Constants.CONSOLIDATION));
    }

    @Test
    void testUpdatePatchContainer_NoContainerIds1() throws RunnerException {
        ContainerV3PatchBulkUpdateRequest request = new ContainerV3PatchBulkUpdateRequest();
        request.setContainerIds(List.of(1L));
        assertThrows(ValidationException.class, () -> containerV3FacadeService.updatePatchContainer(request, Constants.CONSOLIDATION));
    }

    @Test
    void testUpdatePatchContainer_ConsoleIsMismatch() throws RunnerException {
        ContainerV3PatchBulkUpdateRequest request = new ContainerV3PatchBulkUpdateRequest();
        request.setContainerIds(List.of(1L, 2L));
        Containers containers = new Containers();
        containers.setConsolidationId(1L);
        Containers containers1 = new Containers();
        containers1.setConsolidationId(2L);
        when(containerDao.findByIdIn(anyList())).thenReturn(new ArrayList<>(List.of(containers, containers1)));
        assertThrows(ValidationException.class, () -> containerV3FacadeService.updatePatchContainer(request, Constants.CONSOLIDATION));
    }
}
