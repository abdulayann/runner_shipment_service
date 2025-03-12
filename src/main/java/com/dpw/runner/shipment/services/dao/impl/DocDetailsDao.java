package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IDocDetailsDao;
import com.dpw.runner.shipment.services.entity.DocDetails;
import com.dpw.runner.shipment.services.entity.enums.DocDetailsTypes;
import com.dpw.runner.shipment.services.repository.interfaces.IDocDetailsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class DocDetailsDao implements IDocDetailsDao {

    @Autowired
    private IDocDetailsRepository docDetailsRepository;

    @Override
    public DocDetails save(DocDetails docDetails) {
        return docDetailsRepository.save(docDetails);
    }

    @Override
    public List<DocDetails> findByEntityIdAndType(Long entityId, DocDetailsTypes type) {
        return docDetailsRepository.findByEntityIdAndType(entityId, type);
    }
}
