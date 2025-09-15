package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.CommonPackages;

import java.util.List;
import java.util.UUID;

public interface ICommonPackagesDao {
    CommonPackages getByGuid(String guid);

    List<CommonPackages> findByPackingRefGuidIn(List<UUID> guids);

    void saveAll(List<CommonPackages> commonPackagesList);
}
