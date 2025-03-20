package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.DocDetails;
import com.dpw.runner.shipment.services.entity.enums.DocDetailsTypes;

import java.util.List;

public interface IDocDetailsDao {

    DocDetails save(DocDetails docDetails);
    List<DocDetails> findByEntityIdAndType(Long entityId, DocDetailsTypes type);
}
