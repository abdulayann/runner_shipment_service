package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICommonPackagesDao;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.CommonPackages;
import com.dpw.runner.shipment.services.repository.interfaces.ICommonPackageRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.UUID;

@Component
public class CommonPackagesDao implements ICommonPackagesDao {
    @Autowired
    ICommonPackageRepository repository;

    @Override
    public CommonPackages getByGuid(String guid) {
        return repository.getByGuid(guid);
    }

    @Override
    public List<CommonPackages> findByPackingRefGuidIn(List<UUID> guids) {
        return repository.findAllByGuid(guids);
    }

    @Override
    public void saveAll(List<CommonPackages> commonPackagesList){
        repository.saveAll(commonPackagesList);
    }
}
