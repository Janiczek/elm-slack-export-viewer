import fs from 'fs/promises';

const daysFilename = 'days.json';

const channelsDirEntries = await fs.readdir('./export', {withFileTypes: true});

const channels = channelsDirEntries
  .filter(entry => entry.isDirectory())
  .map(entry => entry.name);

channels.forEach(async (channel) => {
  const channelFiles = await fs.readdir(`./export/${channel}`);
  const channelDays = channelFiles
    .filter(file => file !== daysFilename && file.slice(-5) === '.json')
    .map(file => {
      const string = file.slice(0,-5);
      const ymd = string.split('-').map(n => parseInt(n,10));
      return ymd;
    });
  fs.writeFile(`./export/${channel}/${daysFilename}`, JSON.stringify(channelDays));
});
