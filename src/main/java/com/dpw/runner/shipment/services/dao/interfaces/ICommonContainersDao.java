package com.dpw.runner.shipment.services.dao.interfaces;


import com.dpw.runner.shipment.services.entity.CommonContainers;

import java.util.List;
import java.util.UUID;

public interface ICommonContainersDao  {
    CommonContainers getByGuid(UUID guid);

    void saveAll(List<CommonContainers> commonContainersList);

    void save(CommonContainers commonContainers);

    List<CommonContainers> getAll(List<UUID> guids);
}
