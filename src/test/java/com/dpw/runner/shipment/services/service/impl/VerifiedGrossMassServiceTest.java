package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.impl.TransactionHistoryDao;
import com.dpw.runner.shipment.services.dao.impl.VerifiedGrossMassDao;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.TransactionHistoryResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassBulkUpdateRequest;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import com.dpw.runner.shipment.services.entity.enums.WeightDeterminationMethodType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ICommonContainersRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class VerifiedGrossMassServiceTest {

    @Mock
    private ICommonContainersRepository commonContainersRepository;

    @InjectMocks
    private VerifiedGrossMassService verifiedGrossMassService;
    @Mock
    private VerifiedGrossMassDao verifiedGrossMassDao;
    @Mock
    private TransactionHistoryDao transactionHistoryDao;

    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ResponseHelper responseHelper;

    @Test
    void transactionHistoryRetrieveById_shouldThrowException_whenVgmNotFound() {
        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () ->
                verifiedGrossMassService.transactionHistoryRetrieveById(1L)
        );
    }

    @Test
    void transactionHistoryRetrieveById_shouldReturnEmptyList_whenNoHistoryFound() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setStatus(VerifiedGrossMassStatus.Requested);

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(vgm));
        when(transactionHistoryDao.findAllByVerifiedGrossMassId(1L)).thenReturn(Collections.emptyList());

        ResponseEntity<IRunnerResponse> response = verifiedGrossMassService.transactionHistoryRetrieveById(1L);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void transactionHistoryRetrieveById_shouldReturnResponseWithHistory() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setStatus(VerifiedGrossMassStatus.ConfirmedByCarrier);

        TransactionHistory history = new TransactionHistory();
        history.setId(1L);
        history.setDescription("Test Desc");

        TransactionHistoryResponse mockResponse = new TransactionHistoryResponse();
        mockResponse.setId(1L);
        mockResponse.setDescription("Test Desc");
        mockResponse.setActionStatusDescription("Confirmed By Carrier");

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(vgm));
        when(transactionHistoryDao.findAllByVerifiedGrossMassId(1L)).thenReturn(List.of(history));
        when(jsonHelper.convertValue(eq(history), eq(TransactionHistoryResponse.class))).thenReturn(mockResponse);

        ResponseEntity<IRunnerResponse> response = verifiedGrossMassService.transactionHistoryRetrieveById(1L);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());

        RunnerListResponse runnerListResponse = (RunnerListResponse) response.getBody();
        assertNotNull(runnerListResponse);
        List<IRunnerResponse> resultList = runnerListResponse.getData();
        assertNotNull(resultList);
        assertEquals(1, resultList.size());

        TransactionHistoryResponse result = (TransactionHistoryResponse) resultList.get(0);
        assertEquals("Confirmed By Carrier", result.getActionStatusDescription());
        assertEquals("Test Desc", result.getDescription());
    }

    @Test
    public void bulkUpdateContainers_ShouldThrowValidationException_WhenLessThanTwoIds() {
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(List.of(1L)); // only 1 id
        assertThrows(ValidationException.class, () ->
                verifiedGrossMassService.bulkUpdateContainers(request)
        );
    }

    @Test
    void bulkUpdateContainers_ShouldUpdateSuccessfully() {
        // Arrange
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(Arrays.asList(1L, 2L));
        request.setApprovalSignature("abc");
        // create weighingParty
        Parties party = new Parties();
        party.setId(1001L);
        party.setOrgCode("ORG123");
        party.setType("SHIPPER");
        request.setWeighingParty(party);

        CommonContainers c1 = new CommonContainers();
        c1.setId(1L);
        CommonContainers c2 = new CommonContainers();
        c2.setId(2L);
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(request.getContainerIds()))
                .thenReturn(containers);
        when(commonContainersRepository.saveAll(containers))
                .thenReturn(containers);

        // Mock JsonHelper conversion
        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenAnswer(invocation -> {
                    CommonContainers c = invocation.getArgument(0);
                    CommonContainerResponse r = new CommonContainerResponse();
                    r.setId(c.getId());
                    r.setApprovalSignature(c.getApprovalSignature());
                    r.setWeighingParty(c.getWeighingParty());
                    return r;
                });

        // Act
        List<CommonContainerResponse> result = verifiedGrossMassService.bulkUpdateContainers(request);

        // Assert
        assertEquals(2, result.size());
        assertEquals("ABC", result.get(0).getApprovalSignature()); // signature should be uppercase
        assertEquals("ORG123", result.get(0).getWeighingParty().getOrgCode());
    }


    @Test
    void bulkUpdateContainers_ShouldThrowValidationException_WhenContainerNotFound() {
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(Arrays.asList(1L, 2L));

        when(commonContainersRepository.findAllByIdIn(request.getContainerIds()))
                .thenReturn(Collections.singletonList(new CommonContainers())); // only 1 returned

        assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.bulkUpdateContainers(request));
    }

    @Test
    void bulkUpdateContainers_ShouldUpdateApprovalDate() {
        List<Long> ids = Arrays.asList(1L, 2L);
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(ids);
        request.setApprovalDate(LocalDateTime.of(2025, 1, 1, 12, 0));

        CommonContainers c1 = new CommonContainers();
        CommonContainers c2 = new CommonContainers();
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(ids)).thenReturn(containers);
        when(commonContainersRepository.saveAll(anyList())).thenReturn(containers);

        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenReturn(new CommonContainerResponse());

        List<CommonContainerResponse> responses = verifiedGrossMassService.bulkUpdateContainers(request);

        assertEquals(LocalDateTime.of(2025, 1, 1, 12, 0), containers.get(0).getApprovalDate());
        assertEquals(LocalDateTime.of(2025, 1, 1, 12, 0), containers.get(1).getApprovalDate());
        assertEquals(2, responses.size());
    }

    @Test
    void bulkUpdateContainers_ShouldUpdateOnlyProvidedFields() {
        // Arrange
        List<Long> ids = Arrays.asList(1L, 2L);
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(ids);
        request.setApprovalSignature("john"); // only updating signature

        CommonContainers c1 = new CommonContainers();
        CommonContainers c2 = new CommonContainers();
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(ids)).thenReturn(containers);
        when(commonContainersRepository.saveAll(anyList())).thenReturn(containers);

        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenReturn(new CommonContainerResponse());

        // Act
        List<CommonContainerResponse> responses = verifiedGrossMassService.bulkUpdateContainers(request);

        // Assert
        assertEquals("JOHN", containers.get(0).getApprovalSignature()); // uppercase applied
        assertEquals("JOHN", containers.get(1).getApprovalSignature());
        assertEquals(2, responses.size());
    }
    @Test
    void bulkUpdateContainers_ShouldUpdateWeightDeterminationFields() {
        // Arrange
        List<Long> ids = Arrays.asList(1L, 2L);
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(ids);
        request.setWeightDeterminationMethod(WeightDeterminationMethodType.METHOD1);
        request.setWeightDeterminationLocation("Port of LA");

        CommonContainers c1 = new CommonContainers();
        CommonContainers c2 = new CommonContainers();
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(ids)).thenReturn(containers);
        when(commonContainersRepository.saveAll(anyList())).thenReturn(containers);

        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenReturn(new CommonContainerResponse());

        // Act
        List<CommonContainerResponse> responses = verifiedGrossMassService.bulkUpdateContainers(request);

        // Assert
        assertEquals(WeightDeterminationMethodType.METHOD1, c1.getWeightDeterminationMethod());
        assertEquals("Port of LA", c1.getWeightDeterminationLocation());
        assertEquals(WeightDeterminationMethodType.METHOD1, c2.getWeightDeterminationMethod());
        assertEquals("Port of LA", c2.getWeightDeterminationLocation());
        assertEquals(2, responses.size());

        // Verify saveAll called once
        verify(commonContainersRepository, times(1)).saveAll(containers);
    }
}
