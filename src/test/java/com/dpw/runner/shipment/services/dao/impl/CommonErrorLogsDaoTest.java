package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.CommonErrorLogs;
import com.dpw.runner.shipment.services.entity.enums.CommonErrorType;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsoleValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentValidationResponse;
import com.dpw.runner.shipment.services.repository.interfaces.ICommonErrorLogsRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CommonErrorLogsDaoTest {
    @Mock
    private ICommonErrorLogsRepository commonErrorLogsRepository;
    @InjectMocks
    private CommonErrorLogsDao commonErrorLogsDao;

    private static CommonErrorLogs commonErrorLogs;

    @BeforeEach
    void setUp() {
        commonErrorLogs = CommonErrorLogs.builder().entityId(1L).entityType(Constants.SHIPMENT)
                .errorType(CommonErrorType.AUTOMATIC_TRANSFER)
                .errorMessage("AutomaticTransferError").build();
        commonErrorLogs.setId(11L);
        commonErrorLogs.setGuid(UUID.fromString("893cc8fa-7315-4d23-a635-3ce8705a5140"));
    }

    @Test
    void testDeleteAllById() {
        commonErrorLogsDao.deleteAllById(anyList());
        verify(commonErrorLogsRepository, times(1)).deleteAllById(anyList());
    }

    @Test
    void testFindById() {
        Long notificationId = 11L;
        when(commonErrorLogsRepository.findById(notificationId)).thenReturn(Optional.of(commonErrorLogs));
        Optional<CommonErrorLogs> commonErrorLogs1 = commonErrorLogsDao.findById(notificationId);
        verify(commonErrorLogsRepository, times(1)).findById(notificationId);
        assertEquals(notificationId, commonErrorLogs1.get().getId());
    }

    @Test
    void testFindByGuid() {
        UUID randomGuid = UUID.fromString("893cc8fa-7315-4d23-a635-3ce8705a5140");
        when(commonErrorLogsRepository.findByGuid(randomGuid)).thenReturn(Optional.of(commonErrorLogs));
        Optional<CommonErrorLogs> commonErrorLogs1 = commonErrorLogsDao.findByGuid(randomGuid);
        verify(commonErrorLogsRepository, times(1)).findByGuid(randomGuid);
        assertEquals(randomGuid, commonErrorLogs1.get().getGuid());
    }

    @Test
    void testFindAll() {
        Specification<CommonErrorLogs> spec = null;
        Pageable pageable = null;
        List<CommonErrorLogs> commonErrorLogsList = new ArrayList<>();
        Page<CommonErrorLogs> commonErrorLogsPage = new PageImpl<>(commonErrorLogsList);
        when(commonErrorLogsRepository.findAll(spec, pageable)).thenReturn(commonErrorLogsPage);
        Page<CommonErrorLogs> notifications = commonErrorLogsDao.findAll(spec, pageable);
        assertEquals(commonErrorLogsPage.getTotalElements(), notifications.getTotalElements());
    }

    @Test
    void testSaveSuccess() {
        CommonErrorLogs commonErrorLogs1 = new CommonErrorLogs();
        when(commonErrorLogsRepository.save(any())).thenReturn(commonErrorLogs1);
        CommonErrorLogs commonErrorLogs2 = commonErrorLogsDao.save(commonErrorLogs1);
        assertEquals(commonErrorLogs1, commonErrorLogs2);
    }

    @Test
    void testDeleteAllConsoleAndShipmentErrorsLogs() {
        CommonErrorLogs commonErrorLogs1 = commonErrorLogs;
        commonErrorLogs1.setId(12L);

        when(commonErrorLogsRepository.findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any()))
                .thenReturn(List.of(commonErrorLogs));
        when(commonErrorLogsRepository.findByEntityIdListAndEntityTypeAndErrorType(anyList(), anyString(), anyString()))
                .thenReturn(List.of(commonErrorLogs1));
        doNothing().when(commonErrorLogsRepository).deleteAllById(anyList());

        commonErrorLogsDao.deleteAllConsoleAndShipmentErrorsLogs(1L, List.of(1L));

        verify(commonErrorLogsRepository, times(1)).findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any());
        verify(commonErrorLogsRepository, times(1)).findByEntityIdListAndEntityTypeAndErrorType(anyList(), anyString(), anyString());
        verify(commonErrorLogsRepository, times(1)).deleteAllById(anyList());
    }

    @Test
    void testLogShipmentAutomaticTransferErrors() {
        SendShipmentValidationResponse sendShipmentValidationResponse = SendShipmentValidationResponse.builder().build();

        when(commonErrorLogsRepository.findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any()))
                .thenReturn(Collections.emptyList());
        when(commonErrorLogsRepository.save(any(CommonErrorLogs.class))).thenReturn(commonErrorLogs);

        commonErrorLogsDao.logShipmentAutomaticTransferErrors(sendShipmentValidationResponse, 1L);

        verify(commonErrorLogsRepository, times(1)).findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any());
        verify(commonErrorLogsRepository, times(1)).save(any(CommonErrorLogs.class));
    }

    @Test
    void testLogShipmentAutomaticTransferErrors_ExistingErrors() {
        SendShipmentValidationResponse sendShipmentValidationResponse = SendShipmentValidationResponse.builder().build();

        when(commonErrorLogsRepository.findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any()))
                .thenReturn(List.of(commonErrorLogs));
        when(commonErrorLogsRepository.save(any(CommonErrorLogs.class))).thenReturn(commonErrorLogs);

        commonErrorLogsDao.logShipmentAutomaticTransferErrors(sendShipmentValidationResponse, 1L);

        verify(commonErrorLogsRepository, times(1)).findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any());
        verify(commonErrorLogsRepository, times(1)).save(any(CommonErrorLogs.class));
    }

    @Test
    void testDeleteShipmentErrorsLogs() {
        when(commonErrorLogsRepository.findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any()))
                .thenReturn(List.of(commonErrorLogs));
        doNothing().when(commonErrorLogsRepository).deleteById(anyLong());

        commonErrorLogsDao.deleteShipmentErrorsLogs(1L);

        verify(commonErrorLogsRepository, times(1)).findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any());
        verify(commonErrorLogsRepository, times(1)).deleteById(anyLong());
    }

    @Test
    void testLogConsoleAutomaticTransferErrors_WithConsoleErrorAndNoPreviousErrors() {
        Long consoleId = 1L;
        List<Long> shipmentIds = Arrays.asList(101L, 102L);
        SendConsoleValidationResponse response = new SendConsoleValidationResponse();
        response.setIsError(true);
        response.setConsoleErrorMessage("Console error occurred.");
        response.setShipmentErrorMessage("Shipment error occurred.");
        response.setShipmentIds(shipmentIds);

        when(commonErrorLogsRepository.findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), anyString()))
                .thenReturn(Collections.emptyList());
        when(commonErrorLogsRepository.findByEntityIdListAndEntityTypeAndErrorType(anyList(), anyString(), anyString()))
                .thenReturn(Collections.emptyList());
        when(commonErrorLogsRepository.save(any())).thenReturn(commonErrorLogs);
        when(commonErrorLogsRepository.saveAll(anyList())).thenReturn(List.of(commonErrorLogs));

        commonErrorLogsDao.logConsoleAutomaticTransferErrors(response, consoleId, shipmentIds);

        verify(commonErrorLogsRepository, times(1)).findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any());
        verify(commonErrorLogsRepository, times(1)).findByEntityIdListAndEntityTypeAndErrorType(anyList(), anyString(), anyString());
        verify(commonErrorLogsRepository, times(1)).save(any());
        verify(commonErrorLogsRepository, times(1)).saveAll(anyList());
    }

    @Test
    void testLogConsoleAutomaticTransferErrors_WithExistingErrors() {
        Long consoleId = 2L;
        List<Long> shipmentIds = Arrays.asList(201L, 202L);
        SendConsoleValidationResponse response = new SendConsoleValidationResponse();
        response.setIsError(true);
        response.setConsoleErrorMessage("Updated console error occurred.");
        response.setShipmentErrorMessage("Updated shipment error occurred.");
        response.setShipmentIds(Arrays.asList(201L, 202L));

        CommonErrorLogs existingConsoleError = CommonErrorLogs.builder()
                .entityId(consoleId)
                .entityType(Constants.CONSOLIDATION)
                .errorType(CommonErrorType.AUTOMATIC_TRANSFER)
                .errorMessage("Old console error")
                .build();

        CommonErrorLogs existingShipmentError = CommonErrorLogs.builder()
                .entityId(201L)
                .entityType(Constants.SHIPMENT)
                .errorType(CommonErrorType.AUTOMATIC_TRANSFER)
                .errorMessage("Old shipment error")
                .build();

        when(commonErrorLogsRepository.findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), anyString()))
                .thenReturn(Collections.singletonList(existingConsoleError));
        when(commonErrorLogsRepository.findByEntityIdListAndEntityTypeAndErrorType(anyList(), anyString(), anyString()))
                .thenReturn(Collections.singletonList(existingShipmentError));
        when(commonErrorLogsRepository.save(any())).thenReturn(commonErrorLogs);
        when(commonErrorLogsRepository.saveAll(anyList())).thenReturn(List.of(commonErrorLogs));

        commonErrorLogsDao.logConsoleAutomaticTransferErrors(response, consoleId, shipmentIds);

        verify(commonErrorLogsRepository, times(1)).findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any());
        verify(commonErrorLogsRepository, times(1)).findByEntityIdListAndEntityTypeAndErrorType(anyList(), anyString(), anyString());
        verify(commonErrorLogsRepository, times(1)).save(any());
        verify(commonErrorLogsRepository, times(1)).saveAll(anyList());
    }

    @Test
    void testLogConsoleAutomaticTransferErrors_NoErrorScenario() {
        SendConsoleValidationResponse response = new SendConsoleValidationResponse();
        response.setIsError(false);

        commonErrorLogsDao.logConsoleAutomaticTransferErrors(response, 1L, Collections.emptyList());

        verifyNoInteractions(commonErrorLogsRepository);
    }

    @Test
    void testLogConsoleAutomaticTransferErrors_DeleteOldErrors() {
        Long consoleId = 3L;
        List<Long> shipmentIds = Arrays.asList(301L, 302L);
        SendConsoleValidationResponse response = new SendConsoleValidationResponse();
        response.setIsError(true);
        response.setConsoleErrorMessage("New console error occurred.");
        response.setShipmentErrorMessage("New shipment error occurred.");
        response.setShipmentIds(Collections.singletonList(301L));

        CommonErrorLogs oldShipmentError = CommonErrorLogs.builder()
                .entityId(302L)
                .entityType(Constants.SHIPMENT)
                .errorType(CommonErrorType.AUTOMATIC_TRANSFER)
                .errorMessage("Old shipment error")
                .build();
        oldShipmentError.setId(1L);

        when(commonErrorLogsRepository.findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), anyString()))
                .thenReturn(Collections.emptyList());
        when(commonErrorLogsRepository.findByEntityIdListAndEntityTypeAndErrorType(anyList(), anyString(), anyString()))
                .thenReturn(Collections.singletonList(oldShipmentError));
        when(commonErrorLogsRepository.save(any())).thenReturn(commonErrorLogs);
        when(commonErrorLogsRepository.saveAll(anyList())).thenReturn(List.of(commonErrorLogs));
        doNothing().when(commonErrorLogsRepository).deleteAllById(anyList());

        commonErrorLogsDao.logConsoleAutomaticTransferErrors(response, consoleId, shipmentIds);

        verify(commonErrorLogsRepository, times(1)).findByEntityIdAndEntityTypeAndErrorType(anyLong(), anyString(), any());
        verify(commonErrorLogsRepository, times(1)).findByEntityIdListAndEntityTypeAndErrorType(anyList(), anyString(), anyString());
        verify(commonErrorLogsRepository, times(1)).save(any());
        verify(commonErrorLogsRepository, times(1)).saveAll(anyList());
        verify(commonErrorLogsRepository, times(1)).deleteAllById(anyList());
    }

}