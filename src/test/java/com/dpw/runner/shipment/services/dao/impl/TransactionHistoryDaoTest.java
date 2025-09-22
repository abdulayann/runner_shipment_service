package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.repository.interfaces.ITransactionHistoryRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class TransactionHistoryDaoTest {

    private ITransactionHistoryRepository transactionHistoryRepository;
    private TransactionHistoryDao transactionHistoryDao;

    @BeforeEach
    void setUp() {
        transactionHistoryRepository = mock(ITransactionHistoryRepository.class);
        transactionHistoryDao = new TransactionHistoryDao(transactionHistoryRepository);
    }

    @Test
    void testFindById_ShouldReturnOptional() {
        TransactionHistory mockHistory = new TransactionHistory();
        mockHistory.setId(1L);

        when(transactionHistoryRepository.findById(1L)).thenReturn(Optional.of(mockHistory));

        Optional<TransactionHistory> result = transactionHistoryDao.findById(1L);

        assertTrue(result.isPresent());
        assertEquals(1L, result.get().getId());
    }

    @Test
    void testFindAll_ShouldReturnPage() {
        TransactionHistory mockHistory = new TransactionHistory();
        Page<TransactionHistory> mockPage = new PageImpl<>(List.of(mockHistory));

        @SuppressWarnings("unchecked")
        Specification<TransactionHistory> spec = mock(Specification.class);
        Pageable pageable = Pageable.unpaged();

        when(transactionHistoryRepository.findAll(spec, pageable)).thenReturn(mockPage);

        Page<TransactionHistory> result = transactionHistoryDao.findAll(spec, pageable);

        assertNotNull(result);
        assertEquals(1, result.getTotalElements());
    }

    @Test
    void testFindAllByEntityIdAndEntityType_ShouldReturnList() {
        TransactionHistory mockHistory = new TransactionHistory();

        when(transactionHistoryRepository.findAllByEntityIdAndEntityType(1L, "VGM", 100)).thenReturn(List.of(mockHistory));

        List<TransactionHistory> result = transactionHistoryDao.findAllByEntityIdAndEntityType(1L, "VGM", 100);

        assertEquals(1, result.size());
    }

    @Test
    void testUpdate_ShouldSaveAndReturnTransactionHistory() {
        TransactionHistory historyToUpdate = new TransactionHistory();
        historyToUpdate.setId(5L);

        when(transactionHistoryRepository.save(historyToUpdate)).thenReturn(historyToUpdate);

        TransactionHistory result = transactionHistoryDao.update(5L, historyToUpdate);

        assertEquals(5L, result.getId());
    }

    @Test
    void testDelete_ShouldDeleteWhenIdExists() {
        TransactionHistory mockHistory = new TransactionHistory();
        mockHistory.setId(9L);

        when(transactionHistoryRepository.findById(9L)).thenReturn(Optional.of(mockHistory));

        transactionHistoryDao.delete(9L);

        verify(transactionHistoryRepository, times(1)).deleteById(9L);
    }

    @Test
    void testDelete_ShouldThrowWhenIdNotFound() {
        when(transactionHistoryRepository.findById(99L)).thenReturn(Optional.empty());

        assertThrows(ValidationException.class, () -> transactionHistoryDao.delete(99L));
    }

    @Test
    void testSave_ShouldCallRepositorySave() {
        TransactionHistory mockHistory = new TransactionHistory();
        mockHistory.setDescription("test");

        when(transactionHistoryRepository.save(mockHistory)).thenReturn(mockHistory);

        TransactionHistory result = transactionHistoryDao.save(mockHistory);

        assertEquals("test", result.getDescription());
    }
}
