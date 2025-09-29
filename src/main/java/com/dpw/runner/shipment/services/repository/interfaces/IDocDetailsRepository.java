package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.DocDetails;
import com.dpw.runner.shipment.services.entity.enums.DocDetailsTypes;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface IDocDetailsRepository extends MultiTenancyRepository<DocDetails> {

    List<DocDetails> findByEntityIdAndType(Long entityId, DocDetailsTypes type);

    @Query(value = "SELECT * FROM doc_details dd WHERE dd.file_id = ?1 and dd.is_deleted = false", nativeQuery = true)
    DocDetails findByFileId(String fileId);
}
