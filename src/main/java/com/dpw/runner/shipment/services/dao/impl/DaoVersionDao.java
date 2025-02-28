package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IDocVersionDao;
import com.dpw.runner.shipment.services.entity.DocVersion;
import com.dpw.runner.shipment.services.entity.enums.DocVersionTypes;
import com.dpw.runner.shipment.services.repository.interfaces.IDocVersionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class DaoVersionDao implements IDocVersionDao {

    @Autowired
    private IDocVersionRepository docVersionRepository;

    @Override
    public DocVersion save(DocVersion docVersion) {
        return docVersionRepository.save(docVersion);
    }

    @Override
    public List<DocVersion> findByEntityIdAndType(Long entityId, DocVersionTypes type) {
        return docVersionRepository.findByEntityIdAndType(entityId, type);
    }
}
