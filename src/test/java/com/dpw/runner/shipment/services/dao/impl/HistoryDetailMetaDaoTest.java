package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.HistoryDetailMeta;
import com.dpw.runner.shipment.services.repository.interfaces.IHistoryDetailMetaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class HistoryDetailMetaDaoTest {
    @Mock
    private IHistoryDetailMetaRepository historyDetailMetaRepository;

    @InjectMocks
    private HistoryDetailMetaDao historyDetailMetaDao;

    private HistoryDetailMeta sampleHistoryMeta;

    @BeforeEach
    void setup() {
        sampleHistoryMeta = HistoryDetailMeta.builder()
                .entityType("Events")
                .entityId(100L)
                .changedByUser("testUser")
                .changedByUserEmail("test@example.com")
                .changeSource("UI")
                .changeTimestamp(LocalDateTime.now())
                .action("CREATED")
                .build();
        sampleHistoryMeta.setId(1L);
    }

    @Test
    void testSaveAll() {
        List<HistoryDetailMeta> historyMetaList = List.of(sampleHistoryMeta);
        when(historyDetailMetaRepository.saveAll(anyList())).thenReturn(historyMetaList);

        List<HistoryDetailMeta> savedList = historyDetailMetaDao.saveAll(historyMetaList);

        assertNotNull(savedList);
        assertEquals(1, savedList.size());
        assertEquals("Events", savedList.get(0).getEntityType());
        verify(historyDetailMetaRepository, times(1)).saveAll(historyMetaList);
    }
}
