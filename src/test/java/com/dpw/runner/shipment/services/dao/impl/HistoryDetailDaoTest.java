package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.HistoryDetail;
import com.dpw.runner.shipment.services.repository.interfaces.IHistoryDetailRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class HistoryDetailDaoTest {

    @Mock
    private IHistoryDetailRepository historyDetailRepository;

    @InjectMocks
    private HistoryDetailDao historyDetailDao;

    private HistoryDetail sampleDetail;

    @BeforeEach
    void setup() {
        sampleDetail = HistoryDetail.builder()
                .fieldName("Estimated Date")
                .oldValue("16 Oct 2025, 04:00pm")
                .newValue("17 Oct 2025, 06:00am")
                .build();
        sampleDetail.setId(1L);
    }

    @Test
    void testFindByHistoryIdFound() {
        when(historyDetailRepository.findById(1L)).thenReturn(Optional.of(sampleDetail));

        Optional<HistoryDetail> result = historyDetailDao.findByHistoryId(1L);

        assertTrue(result.isPresent());
        assertEquals("Estimated Date", result.get().getFieldName());
        verify(historyDetailRepository, times(1)).findById(1L);
    }

    @Test
    void testFindByHistoryIdNotFound() {
        when(historyDetailRepository.findById(2L)).thenReturn(Optional.empty());

        Optional<HistoryDetail> result = historyDetailDao.findByHistoryId(2L);

        Assertions.assertFalse(result.isPresent());
        verify(historyDetailRepository, times(1)).findById(2L);
    }

    @Test
    void testSave() {
        when(historyDetailRepository.save(any(HistoryDetail.class))).thenReturn(sampleDetail);

        HistoryDetail saved = historyDetailDao.save(sampleDetail);

        assertNotNull(saved);
        assertEquals("Estimated Date", saved.getFieldName());
        verify(historyDetailRepository, times(1)).save(sampleDetail);
    }
}
