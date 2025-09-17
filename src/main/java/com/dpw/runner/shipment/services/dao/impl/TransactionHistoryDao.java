package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.repository.interfaces.ITransactionHistoryRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
public class TransactionHistoryDao implements ITransactionHistoryDao {

    private final ITransactionHistoryRepository transactionHistoryRepository;

    public TransactionHistoryDao(ITransactionHistoryRepository transactionHistoryRepository) {
        this.transactionHistoryRepository = transactionHistoryRepository;
    }

    @Override
    public Optional<TransactionHistory> findById(Long id) {
        return transactionHistoryRepository.findById(id);
    }

    @Override
    public Page<TransactionHistory> findAll(Specification<TransactionHistory> spec, Pageable pageable) {
        return transactionHistoryRepository.findAll(spec, pageable);
    }

    @Override
    public List<TransactionHistory> findAllByEntityIdAndEntityType(Long entityId, String entityType, Integer tenantId) {
        return transactionHistoryRepository.findAllByEntityIdAndEntityType(entityId, entityType, tenantId);
    }

    @Override
    public TransactionHistory update(Long id, TransactionHistory transactionHistory) {
        return transactionHistoryRepository.save(transactionHistory);
    }

    @Override
    public void delete(Long id) {
        Optional<TransactionHistory> transactionHistory = transactionHistoryRepository.findById(id);
        if (transactionHistory.isEmpty()) {
            throw new ValidationException("Invalid transactionalHistory id");
        }
        transactionHistoryRepository.deleteById(id);
    }

    @Override
    public TransactionHistory save(TransactionHistory transactionHistory) {
        return transactionHistoryRepository.save(transactionHistory);
    }
}
