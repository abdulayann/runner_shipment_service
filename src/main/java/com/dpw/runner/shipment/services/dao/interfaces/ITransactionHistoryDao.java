package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface ITransactionHistoryDao {

    Optional<TransactionHistory> findById(Long id);

    Page<TransactionHistory> findAll(Specification<TransactionHistory> spec, Pageable pageable);

    TransactionHistory update(Long id, TransactionHistory transactionHistory);

    void delete(Long id);

    TransactionHistory save(TransactionHistory transactionHistory);

    List<TransactionHistory> findAllByEntityIdAndEntityType(Long entityId, String entityType, Integer tenantId);
}
