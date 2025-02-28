package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.DocVersion;
import com.dpw.runner.shipment.services.entity.enums.DocVersionTypes;

import java.util.List;

public interface IDocVersionDao {

    DocVersion save(DocVersion docVersion);
    List<DocVersion> findByEntityIdAndType(Long entityId, DocVersionTypes type);
}
