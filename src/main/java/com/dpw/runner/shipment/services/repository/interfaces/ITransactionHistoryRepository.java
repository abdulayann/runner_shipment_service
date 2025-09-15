package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import org.springframework.data.repository.query.Param;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface ITransactionHistoryRepository extends MultiTenancyRepository<TransactionHistory> {

    // This method will return a list of transaction histories for a given verifiedGrossMassId
    @Query(value = "SELECT * FROM transaction_history th WHERE th.entity_id = :entityId AND th.entity_type = :entityType", nativeQuery = true)
    List<TransactionHistory> findAllByEntityIdAndEntityType(@Param("entityId") Long entityId, @Param("entityType") String entityType);

}
