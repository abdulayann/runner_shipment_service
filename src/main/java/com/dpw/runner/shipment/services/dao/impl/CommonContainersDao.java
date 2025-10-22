package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICommonContainersDao;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.repository.interfaces.ICommonContainerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.UUID;

@Component
public class CommonContainersDao implements ICommonContainersDao {

    @Autowired
    ICommonContainerRepository repository;

    @Override
    public CommonContainers getByGuid(UUID guid) {
        return repository.getByGuid(guid);
    }

    @Override
    public void saveAll(List<CommonContainers> commonContainersList) {
        repository.saveAll(commonContainersList);
    }

    @Override
    public void save(CommonContainers commonContainer) {
        repository.save(commonContainer);
    }

    @Override
    public List<CommonContainers> getAll(List<UUID> guids) {
        return repository.findAllByGuid(guids);
    }
}
